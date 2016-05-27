package net.nomadicalien.nlp.actor

import akka.NotUsed
import akka.actor._
import akka.stream._
import akka.stream.scaladsl._
import net.nomadicalien.nlp.{Logging, NaturalLanguageProcessor, ProbFormatter, Sentence}

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Await, Promise}
import scala.util.{Success, Random}

class ActorStreamNLP(stringToDecode: String, solution: String) extends NaturalLanguageProcessor with Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)

  implicit val actorSystem = ActorSystem("NLP")
  implicit val materializer = ActorMaterializer()

  val reconciler = actorSystem.actorOf(Props(classOf[StreamSentenceComparatorReconciler], encryptedSentence, solutionSentence))

  val perms = () => ('a' to 'z').toList.permutations
  val permSource: Source[List[Char], NotUsed] = Source.fromIterator[List[Char]](perms)
  val sentenceSource: Source[Sentence, NotUsed] =
    permSource.async
      .map {perm :List[Char] =>
      val zipped = encryptedSentence.distinctLetters zip perm.reverse
      encryptedSentence.swapMultiple(zipped)
    }

  val sentenceFlow: Flow[List[Char], Sentence, NotUsed] =
    Flow[List[Char]].map { perm =>
      val zipped = encryptedSentence.distinctLetters zip perm.reverse
      encryptedSentence.swapMultiple(zipped)
    }

  import GraphDSL.Implicits._
  val sentenceGenerator: Flow[List[Char], Sentence, NotUsed] = Flow.fromGraph(GraphDSL.create() { implicit builder =>
    val workerCount = 5
    val balancer = builder.add(Balance[List[Char]](workerCount))
    val merge = builder.add(Merge[Sentence](workerCount))

    for (_ <- 1 to workerCount) {
      balancer ~> sentenceFlow.async ~> merge
    }

    FlowShape(balancer.in, merge.out)
  })

  val runnable2: RunnableGraph[Future[Sentence]] = permSource.async.via(sentenceGenerator).toMat(Sink.fold(encryptedSentence)(sentenceMax))(Keep.right)
  val runnable: RunnableGraph[Future[Sentence]] = sentenceSource.async.toMat(Sink.fold(encryptedSentence)(sentenceMax))(Keep.right)

  override def process(): Sentence = {
    logger.info(s"ENCRYPTED     [$encryptedSentence]")
    logger.info(s"SOLUTION      [$solutionSentence]")
    logSentence("INITIAL SENTENCE", encryptedSentence)

    val sentenceF: Future[Sentence] = runnable2.run()
    Await.result(sentenceF, Duration.Inf)
  }

  def sentenceMax(lhs: Sentence, rhs: Sentence): Sentence = {
    val newMax = {
      if (lhs.probabilityCorrect > rhs.probabilityCorrect) {
        lhs
      } else {
        rhs
      }
    }

    reconciler ! NewMax(newMax)

    newMax
  }

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }
}

class StreamSentenceComparatorReconciler(encryptedSentence: Sentence, solutionSentence: Sentence) extends Actor with Logging {
  var currentMax: Sentence = encryptedSentence
  val random = new Random()

  override def receive: Actor.Receive = {
    case NewMax(s) =>
      if(random.nextInt(10000000) == 0) {
        logSentence("RECONCILED SANITY CHECK", s)
      }

      if(s.probabilityCorrect > currentMax.probabilityCorrect) {
        logSentence("NEW RECONCILED MAX", s)
        currentMax = s
      }

      if(solutionSentence == s) {
        logger.info("!!!---Found it---!!!")
      }
  }

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }

}
