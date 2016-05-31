package net.nomadicalien.nlp.actor

import java.security.SecureRandom
import java.util.concurrent.TimeUnit

import akka.NotUsed
import akka.actor._
import akka.stream._
import akka.stream.actor.ActorPublisherMessage.{Cancel, Request}
import akka.stream.actor.ActorSubscriberMessage.OnNext
import akka.stream.actor.{ActorPublisher, ActorSubscriber, MaxInFlightRequestStrategy}
import akka.stream.scaladsl._
import net.nomadicalien.nlp._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration.{FiniteDuration, Duration}
import scala.util.Random

case class ReplacePool(newPool: List[Sentence])

class ActorSentencePermSource(val encryptedSentence: Sentence) extends ActorPublisher[Sentence] with Logging {

  context.system.eventStream.subscribe(self, classOf[ReplacePool])

  val rng = new Random(new SecureRandom())
  var pool: List[Sentence] = List(encryptedSentence)

  val letterPool: Array[Letter] = ('a' to 'z').toArray
  val numLetters = 26
  @tailrec
  final def sampleIndexedPool(n: Int, poolSize: Int, samples: List[Int] = List.empty[Int]): List[Int] = {
    if(n == samples.size) {
      samples
    } else {
      sampleIndexedPool(n, poolSize, rng.nextInt(poolSize) :: samples)
    }
  }

  def randomSample(base: Sentence): Sentence = {
    val letters = base.distinctLetters
    sampleDistinctLetters(letters, base)
  }

  @tailrec
  final def sampleLetters(sampleSize: Int, letters:List[Letter], samples: List[Letter] = List.empty[Letter]):List[Letter] = {
    if(samples.size == sampleSize || samples.size == letters.size) {
      samples
    } else {
      val nextSampleNum = rng.nextInt(letters.size)
      val sample = letters(nextSampleNum)
      val remainingLetters = letters.filterNot(l => l == sample)
      sampleLetters(sampleSize, remainingLetters, sample::samples)
    }
  }

  def sampleDistinctLetters(distinctLetters: List[Letter], base: Sentence) = {
    val numDistinctLetters = distinctLetters.size
    val numReplacements: Int = rng.nextInt(math.max(numDistinctLetters/2, 1)) + 1
    val losingLetters = sampleLetters(numReplacements, distinctLetters)
    val replacementPool = letterPool.filterNot(losingLetters.contains).toList
    val replacementLetters = sampleLetters(losingLetters.size, replacementPool)
    base.swapMultiple((losingLetters zip replacementLetters).toMap)
  }

  def leastLikelyWordSample(base: Sentence): Sentence = {
    val letters = base.findLeastLikelyWord().letters.toList.distinct
    sampleDistinctLetters(letters, base)
  }



  def receive = {
    case Request(_) =>
      deliverBuf()
    case Cancel =>
      context.stop(self)
    case ReplacePool(newPool) =>
      pool = newPool
  }

  @tailrec
  final def deliverBuf(): Unit = {
    if (totalDemand > 0) {
      if (totalDemand <= Int.MaxValue) {
        fullFillDemand(totalDemand.toInt)
      } else {
        fullFillDemand(Int.MaxValue)
        deliverBuf()
      }
    }
  }

  def fullFillDemand(theDemand: Int): Unit = {
    val groupedSampleTypes = chooseSampleTypes(theDemand).groupBy(s =>s)

    val randomPooledSamples = groupedSampleTypes.getOrElse(RandomPooledSample, List.empty[SamplingType])

    sampleIndexedPool(randomPooledSamples.size, pool.size)
      .map(pool.apply)
      .map(randomSample)
      .foreach(onNext)

    val leastLikelyWordPooledSamples = groupedSampleTypes.getOrElse(LeastLikelyWordPooledSample, List.empty[SamplingType])
    sampleIndexedPool(leastLikelyWordPooledSamples.size, pool.size)
      .map(pool.apply)
      .map(leastLikelyWordSample)
      .foreach(onNext)
  }

  sealed trait SamplingType
  case object RandomPooledSample extends SamplingType
  case object LeastLikelyWordPooledSample extends SamplingType

  def chooseSampleTypes(n: Int): Seq[SamplingType] = {
    Stream.continually {
      val sampleTypeNumber = rng.nextInt(100)
      if(sampleTypeNumber < 10) {
        LeastLikelyWordPooledSample
      } else {
        RandomPooledSample
      }
    }.take(n)
  }

}

class GeneticSelectionActor(val solutionSentence: Sentence) extends ActorSubscriber with Logging{
  import scala.collection.mutable
  val rng = new Random()

  implicit val ordering: Ordering[Sentence] = new Ordering[Sentence] {
    override def compare(x: Sentence, y: Sentence): Int = Ordering.Double.compare(x.probabilityCorrect, y.probabilityCorrect)
  }

  val bestMatchQueue = new mutable.PriorityQueue[Sentence]

  override val requestStrategy = new MaxInFlightRequestStrategy(max = 10) {
    override def inFlightInternally: Int = 0
  }

  def receive = {
    case OnNext(s: Sentence) =>
      if(rng.nextInt(100000) == 0) {
        logSentence("SANITY CHECK", s)
      }

      if(!bestMatchQueue.toSet.contains(s)) {
        bestMatchQueue.enqueue(s)
        if(s == solutionSentence) {
          logSentence("***FOUND IT***", s)
          logger.info("!!shutting down!!!")
          implicit val ec: ExecutionContext = context.system.dispatcher
          context.system.scheduler.scheduleOnce(FiniteDuration(2l, TimeUnit.SECONDS), new Runnable {
            def run() = {context.system.terminate()}
          })

        }
      }


      if(bestMatchQueue.size > 1000) {
        logSentence("BEST", bestMatchQueue.head)
        val best = Stream.continually {
          bestMatchQueue.dequeue()
        }.take(100).toList
        bestMatchQueue.clear()
        bestMatchQueue.enqueue(best :_*)
        context.system.eventStream.publish(ReplacePool(best))
      }
  }

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }
}

class ActorStreamGeneticNLP(stringToDecode: String, solution: String) extends NaturalLanguageProcessor with Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)

  implicit val actorSystem = ActorSystem("NLP")
  implicit val materializer = ActorMaterializer()

  val reconciler = actorSystem.actorOf(Props(classOf[StreamSentenceComparatorReconciler], encryptedSentence, solutionSentence))

  val permSource: Source[Sentence, ActorRef] =
    Source.actorPublisher[Sentence](Props(classOf[ActorSentencePermSource], encryptedSentence))

  val sentenceFlow: Flow[Sentence, Sentence, NotUsed] =
    Flow[Sentence].grouped(10).map { group =>
      group.reduce { (s1: Sentence, s2: Sentence) =>
        if(s1.probabilityCorrect > s2.probabilityCorrect)
          s1
        else
          s2
      }
    }

  import GraphDSL.Implicits._
  val sentenceGenerator: Flow[Sentence, Sentence, NotUsed] = Flow.fromGraph(GraphDSL.create() { implicit builder =>
    val workerCount = 7
    val balancer = builder.add(Balance[Sentence](workerCount))
    val merge = builder.add(Merge[Sentence](workerCount))

    for (_ <- 1 to workerCount) {
      balancer ~> sentenceFlow.async ~> merge
    }

    FlowShape(balancer.in, merge.out)
  })

  val runnable: RunnableGraph[NotUsed] = permSource.async.via(sentenceGenerator).toMat(Sink.actorSubscriber(Props(classOf[GeneticSelectionActor], solutionSentence)))(Keep.none)

  override def process(): Sentence = {
    logger.info(s"ENCRYPTED     [$encryptedSentence]")
    logger.info(s"SOLUTION      [$solutionSentence]")
    logSentence("INITIAL SENTENCE", encryptedSentence)

    runnable.run()
    Await.result(actorSystem.whenTerminated, Duration.Inf)
    encryptedSentence//not right, should ask actor for result after certain time limit
  }

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }
}


