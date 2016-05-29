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
    val numDistinctLetters = letters.size
    val numReplacements: Int = math.max(rng.nextInt((numDistinctLetters/3)),3)
    val losingLetters = sampleIndexedPool(numReplacements, numDistinctLetters).map(letters.apply).toSet
    val loosingSentenceLetters = letters.filter(losingLetters.contains)
    val replacementLetters = sampleIndexedPool(loosingSentenceLetters.size, numLetters).map(letterPool.apply)
        .filterNot( r => losingLetters.contains(r))
    val replacementLettersWithoutDups = replacementLetters.foldLeft(List.empty[Letter]){
      (acc, letter) =>
      if(!acc.contains(letter)) {
        letter :: acc
      } else {
        acc
      }
    }
    base.swapMultiple(loosingSentenceLetters zip replacementLettersWithoutDups)
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
    val (randomSamples, pooledSamples) = chooseSampleTypes(theDemand.toInt).partition(isRandom => isRandom)
    randomSamples.map(_ => randomSample(encryptedSentence)).foreach(onNext)

    sampleIndexedPool(pooledSamples.size, pool.size)
      .map(pool.apply)
      .map(randomSample)
      .foreach(onNext)
  }



  def chooseSampleTypes(n: Int): Seq[Boolean] = {
    Stream.continually {
      rng.nextInt(100) < 10
    }.take(n)
  }

}

class GeneticSelectionActor(val solutionSentence: Sentence) extends ActorSubscriber with Logging{
  import scala.collection.mutable
  val rng = new Random()

  implicit val ordering: Ordering[Sentence] = new Ordering[Sentence] {
    override def compare(x: Sentence, y: Sentence): Int = Ordering.Double.compare(x.probabilityCorrect, y.probabilityCorrect)
  }

  val priorityQueue = new mutable.PriorityQueue[Sentence]

  override val requestStrategy = new MaxInFlightRequestStrategy(max = 1000) {
    override def inFlightInternally: Int = 0
  }

  def receive = {
    case OnNext(s: Sentence) =>
      if(rng.nextInt(10000) == 0) {
        logSentence("SANITY CHECK", s)
      }

      if(!priorityQueue.toSet.contains(s)) {
        priorityQueue.enqueue(s)
        if(s == solutionSentence) {
          logSentence("***FOUND IT***", s)
          logger.info("!!shutting down!!!")
          implicit val ec: ExecutionContext = context.system.dispatcher
          context.system.scheduler.scheduleOnce(FiniteDuration(2l, TimeUnit.SECONDS), new Runnable {
            def run() = {context.system.terminate()}
          })

        }
      }

      if(priorityQueue.size > 1000) {
        logSentence("BEST", priorityQueue.head)
        val best100 = Stream.continually {
          priorityQueue.dequeue()
        }.take(100).toList
        best100.foreach(logSentence("TOP 100", _))
        context.system.eventStream.publish(ReplacePool(best100))
        priorityQueue.clear()
        priorityQueue.enqueue(best100 :_*)
        logSentence("BEST", priorityQueue.head)
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
    Flow[Sentence].grouped(1000).map { group =>
      group.reduce { (s1: Sentence, s2: Sentence) =>
        if(s1.probabilityCorrect > s2.probabilityCorrect)
          s1
        else
          s2
      }
    }

  import GraphDSL.Implicits._
  val sentenceGenerator: Flow[Sentence, Sentence, NotUsed] = Flow.fromGraph(GraphDSL.create() { implicit builder =>
    val workerCount = 5
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


