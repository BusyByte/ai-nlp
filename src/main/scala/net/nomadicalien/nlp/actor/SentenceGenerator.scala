package net.nomadicalien.nlp.actor

import akka.actor.{ActorRef, Actor}
import akka.dispatch.{RequiresMessageQueue, BoundedMessageQueueSemantics}
import net.nomadicalien.nlp.{Logging, Sentence}

/**
 * Created by Shawn on 12/16/2014.
 */
class SentenceGenerator(encryptedSentence: Sentence) extends Actor with Logging with RequiresMessageQueue[BoundedMessageQueueSemantics] {
  val comparator = context.system.actorSelection("/user/SentenceComparator")
  var stepCount:Long = 0

  override def receive: Actor.Receive = {
    case Permutation(perm) =>
      stepCount = stepCount + 1
      if(stepCount % 1000000 == 0) {
        logger.info(s"Current Perm [${perm.mkString}]")
      }
      val zipped = encryptedSentence.distinctLetters zip perm
      comparator ! encryptedSentence.swapMultiple(zipped)
  }
}
