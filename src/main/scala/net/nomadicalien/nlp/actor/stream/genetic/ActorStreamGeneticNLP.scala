package net.nomadicalien.nlp.actor.stream.genetic

import akka.NotUsed
import akka.actor._
import akka.stream._
import akka.stream.scaladsl._
import net.nomadicalien.nlp.Probability._
import net.nomadicalien.nlp.Sentence._
import net.nomadicalien.nlp._
import net.nomadicalien.nlp.actor._
import net.nomadicalien.nlp.actor.stream.StreamSentenceComparatorReconciler

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class ActorStreamGeneticNLP(stringToDecode: String, solution: String) extends NaturalLanguageProcessor with Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)

  implicit val actorSystem = ActorSystem("NLP")
  implicit val materializer = ActorMaterializer()

  val reconciler = actorSystem.actorOf(Props(classOf[StreamSentenceComparatorReconciler], encryptedSentence, solutionSentence))

  val permSource: Source[Sentence, ActorRef] =
    Source.actorPublisher[Sentence](Props(classOf[ActorSentencePermSource], encryptedSentence))

  val sentenceFlow: Flow[Sentence, Sentence, NotUsed] =
    Flow[Sentence].map { sentence =>
      val prob = sentence.probabilityCorrect
      sentence
    }

  import GraphDSL.Implicits._
  val sentenceGenerator: Flow[Sentence, Sentence, NotUsed] = Flow.fromGraph(GraphDSL.create() { implicit builder =>
    val workerCount = numWorkers
    val balancer = builder.add(Balance[Sentence](workerCount))
    val merge = builder.add(Merge[Sentence](workerCount))

    for (_ <- 1 to workerCount) {
      balancer ~> sentenceFlow.async ~> merge
    }

    FlowShape(balancer.in, merge.out)
  })

  val runnable: RunnableGraph[NotUsed] = permSource.async.via(sentenceGenerator).async.toMat(Sink.actorSubscriber(Props(classOf[GeneticSelectionActor], solutionSentence)))(Keep.none)

  override def process(): Sentence = {
    logSentence("INITIAL ENCRYPTED SENTENCE", encryptedSentence)
    logSentence("SOLUTION SENTENCE", solutionSentence)

    runnable.run()

    Await.result(actorSystem.whenTerminated, Duration.Inf)
    encryptedSentence//TODO: not right, should ask actor for result after certain time limit
  }

}


