package net.nomadicalien.nlp.actor

import akka.actor._
import akka.routing._
import net.nomadicalien.nlp.{Logging, NaturalLanguageProcessor, ProbFormatter, Sentence}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.Success

/**
 * Created by Shawn on 12/15/2014.
 */
class ActorNLP(stringToDecode: String, solution: String) extends NaturalLanguageProcessor with Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)
  override def process(): Sentence = {
    logger.info(s"ENCRYPTED     [$encryptedSentence]")
    logger.info(s"SOLUTION      [$solutionSentence]")
    val perms = ('a' to 'z').toList.permutations
    logSentence("INITIAL SENTENCE", encryptedSentence)

    val promise = Promise[Sentence]()

    val actorSystem = ActorSystem("NLP")
    val sentComparator = actorSystem.actorOf(Props(classOf[SentenceComparator], promise, encryptedSentence, solutionSentence).withMailbox(mailbox), "SentenceComparator")
    val permGenerator = actorSystem.actorOf(Props(classOf[PermutationGenerator], encryptedSentence, sentComparator), "PermGen")
    permGenerator ! Start

    val f = promise.future
    Await.result(f, Duration.Inf)
  }

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }
}
