package net.nomadicalien.nlp.actor.stream

import akka.NotUsed
import akka.actor._
import akka.stream._
import akka.stream.scaladsl._
import net.nomadicalien.nlp.actor._
import net.nomadicalien.nlp.{Logging, NaturalLanguageProcessor, Sentence}
import net.nomadicalien.nlp.Sentence._
import net.nomadicalien.nlp.Probability._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class ActorStreamParrallelNLP(stringToDecode: String, solution: String) extends NaturalLanguageProcessor with Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)

  implicit val actorSystem = ActorSystem("NLP")
  implicit val materializer = ActorMaterializer()

  val reconciler = actorSystem.actorOf(Props(classOf[StreamSentenceComparatorReconciler], encryptedSentence, solutionSentence))

  val perms = () => ('a' to 'z').toList.permutations
  val permSource: Source[List[Char], NotUsed] =
    Source.fromIterator[List[Char]](perms)

  val sentenceFlow: Flow[List[Char], Sentence, NotUsed] =
    Flow[List[Char]].map { perm =>
      val zipped = encryptedSentence.distinctLetters zip perm.reverse
      encryptedSentence.swapMultiple(zipped.toMap)
    }

  import GraphDSL.Implicits._
  val sentenceGenerator: Flow[List[Char], Sentence, NotUsed] = Flow.fromGraph(GraphDSL.create() { implicit builder =>
    val workerCount = numWorkers
    val balancer = builder.add(Balance[List[Char]](workerCount))
    val merge = builder.add(Merge[Sentence](workerCount))

    for (_ <- 1 to workerCount) {
      balancer ~> sentenceFlow.async ~> merge
    }

    FlowShape(balancer.in, merge.out)
  })

  val runnable: RunnableGraph[Future[Sentence]] = permSource.async.via(sentenceGenerator).toMat(Sink.fold(encryptedSentence)(sentenceMax))(Keep.right)

  override def process(): Sentence = {

    logSentence("INITIAL ENCRYPTED SENTENCE", encryptedSentence)
    logSentence("SOLUTION SENTENCE", solutionSentence)

    val sentenceF: Future[Sentence] = runnable.run()
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
}


