package net.nomadicalien.nlp.actor

import akka.actor.{DeadLetter, Actor}
import net.nomadicalien.nlp.Logging

/**
 * Created by Shawn on 12/17/2014.
 */
class BackPressureWatcher extends Actor with Logging {

  var deadLetterTime: Option[Long] = None
  val defaultWindowTime = 10
  val minimumWindowTime = 2
  var windowTime = defaultWindowTime

  
  override def receive: Receive = {
    case d: DeadLetter =>
      logger.info(s"Dead Letter $d")

      if(deadLetterTime.isEmpty || isPastWindow()) {
        deadLetterTime = Some(System.currentTimeMillis())
        windowTime = defaultWindowTime
        context.system.eventStream.publish(SlowDown)  
      } else {
        windowTime = scala.math.min(minimumWindowTime, windowTime - 1)
      }

      d.recipient.!(d.message)(d.sender)
  }
  
  def isPastWindow(): Boolean = {
    (System.currentTimeMillis() - deadLetterTime.get) > (windowTime * generationSchedule.toMillis)
  }
  
  
}
