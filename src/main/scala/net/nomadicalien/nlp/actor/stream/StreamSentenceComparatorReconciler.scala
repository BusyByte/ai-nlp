package net.nomadicalien.nlp.actor.stream

import akka.actor.Actor
import net.nomadicalien.nlp.Probability._
import net.nomadicalien.nlp.Sentence._
import net.nomadicalien.nlp.actor.NewMax
import net.nomadicalien.nlp.{Logging, Sentence}

import scala.util.Random

class StreamSentenceComparatorReconciler(encryptedSentence: Sentence, solutionSentence: Sentence) extends Actor with Logging {
  var currentMax: Sentence = encryptedSentence
  val random = new Random()

  def receive: Actor.Receive = {
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

}