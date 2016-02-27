package net.nomadicalien.nlp.actor

import akka.stream.actor.ActorPublisher
import net.nomadicalien.nlp.Logging

import scala.annotation.tailrec

class PermutationGenerator extends ActorPublisher[Permutation] with Logging {
  import akka.stream.actor.ActorPublisherMessage._

  val perms = ('a' to 'z').toList.permutations

  override def receive: Receive = {
    case Request(_) =>
      deliver()
    case Cancel =>
      context.stop(self)
  }


  @tailrec final def deliver(): Unit =
    if (totalDemand > 0) {
      val numToTake: Int = {
        if (totalDemand <= Int.MaxValue) {
          totalDemand.toInt
        } else {
          Int.MaxValue
        }
      }
      perms.take(numToTake).map(Permutation).foreach(onNext)

      deliver()
    }

}
