package net.nomadicalien.nlp.actor

import akka.actor.Actor
import akka.dispatch.{RequiresMessageQueue, BoundedMessageQueueSemantics}
import net.nomadicalien.nlp.{ProbFormatter, Logging, Sentence}

import scala.concurrent.Promise
import scala.util.Success

/**
 * Created by Shawn on 12/16/2014.
 */
class SentenceComparator(val p: Promise[Sentence], encryptedSentence: Sentence, solutionSentence: Sentence) extends Actor with Logging with RequiresMessageQueue[BoundedMessageQueueSemantics] {
  var currentMax: Sentence = encryptedSentence
  var stepCount:Long = 0

  override def receive: Actor.Receive = {
    case s: Sentence =>
      stepCount = stepCount + 1
      if(stepCount % 1000000 == 0) {
        logSentence("SANITY CHECK", s)
      }

      if(s.probabilityCorrect > currentMax.probabilityCorrect) {
        logSentence("NEW MAX", s)
        currentMax = s
      }

      if(solutionSentence == s) {
        logger.info("!!!---Found it---!!!")
      }

    case CompleteResult =>
      p.complete(Success(currentMax))
      context.stop(self)
      context.system.shutdown()
  }

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }

}
