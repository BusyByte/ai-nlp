package net.nomadicalien.nlp.actor

import akka.actor.Actor
import net.nomadicalien.nlp.{Logging, Sentence}

import scala.concurrent.Promise
import scala.util.{Random, Success}
import net.nomadicalien.nlp.Sentence._
import net.nomadicalien.nlp.Probability._

class SentenceComparatorReconciler(p: Promise[Sentence], encryptedSentence: Sentence, solutionSentence: Sentence) extends Actor with Logging {
  var currentMax: Sentence = encryptedSentence
  val random = new Random()

  context.system.eventStream.subscribe(self, classOf[NewMax])
  context.system.eventStream.subscribe(self, classOf[CompleteResult])

  def receive: Actor.Receive = {
    case NewMax(s) =>
      if(random.nextInt(1000000) == 0) {
        logSentence("RECONCILED SANITY CHECK", s)
      }

      if(s.probabilityCorrect > currentMax.probabilityCorrect) {
        logSentence("NEW RECONCILED MAX", s)
        currentMax = s
      }

      if(solutionSentence == s) {
        logger.info("!!!---Found it---!!!")
      }

    case CompleteResult =>
      p.complete(Success(currentMax))
      context.stop(self)
      context.system.terminate()
  }
}
