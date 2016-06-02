package net.nomadicalien.nlp.actor

import akka.actor.Actor
import akka.dispatch.{BoundedMessageQueueSemantics, RequiresMessageQueue}
import net.nomadicalien.nlp.{Logging, ProbFormatter, Sentence}

import scala.concurrent.Promise
import scala.util.{Random, Success}

/**
 * Created by Shawn on 12/16/2014.
 */
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

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }

}
