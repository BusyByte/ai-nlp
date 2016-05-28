package net.nomadicalien.nlp.actor

import akka.actor.Actor
import net.nomadicalien.nlp.{ProbFormatter, Logging, Sentence}

import scala.util.Random

/**
  * Created by Shawn on 5/28/2016.
  */
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