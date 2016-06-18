package net.nomadicalien.nlp.actor

import akka.actor._
import net.nomadicalien.nlp.Sentence._
import net.nomadicalien.nlp.Probability._
import net.nomadicalien.nlp.{Logging, NaturalLanguageProcessor, Sentence}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

class ActorNLP(stringToDecode: String, solution: String) extends NaturalLanguageProcessor with Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)

  override def process(): Sentence = {
    logSentence("INITIAL ENCRYPTED SENTENCE", encryptedSentence)
    logSentence("SOLUTION SENTENCE", solutionSentence)
    val promise = Promise[Sentence]()

    val actorSystem = ActorSystem("NLP")
    val _ = actorSystem.actorOf(Props(classOf[SentenceComparatorReconciler], promise, encryptedSentence, solutionSentence))
    val permGenerator = actorSystem.actorOf(Props(classOf[PermutationGenerator], encryptedSentence), "PermutationGenerator")

    permGenerator ! Start

    val f = promise.future
    Await.result(f, Duration.Inf)
  }
}
