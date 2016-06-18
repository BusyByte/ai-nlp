package net.nomadicalien.nlp.actor

import akka.actor.Actor
import net.nomadicalien.nlp.{Logging, Sentence}
import net.nomadicalien.nlp.Sentence._
import net.nomadicalien.nlp.Probability._


import scala.util.Random

class SentenceComparator extends Actor with Logging {
  var currentMax: Option[Sentence] = None
  val random = new Random()

  override def receive: Actor.Receive = {
    case s: Sentence =>

      if(currentMax.isEmpty || s.probabilityCorrect > currentMax.get.probabilityCorrect) {
        logSentence("NEW MAX", s)
        currentMax = Some(s)
        context.system.eventStream.publish(NewMax(s))
      }

      if(random.nextInt(1000000) == 0) {
        logSentence("SANITY CHECK", s)
      }
  }
}
