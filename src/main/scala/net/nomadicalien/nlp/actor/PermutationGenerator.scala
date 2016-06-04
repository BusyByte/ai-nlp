package net.nomadicalien.nlp.actor

import akka.actor._
import net.nomadicalien.nlp.{Logging, Sentence}

import scala.collection.mutable

class PermutationGenerator(encryptedSentence: Sentence) extends Actor with Logging {
  val workerRegistry = mutable.Set[ActorRef]()
  val perms = ('a' to 'z').toList.permutations

  override def preStart() = (1 to numWorkers).foreach {number => context.actorOf(Props(classOf[SentenceGenerator], encryptedSentence),s"SentenceGenerator$number")}
  override def receive: Receive = {
    case Start =>

    case RegisterWorker =>
      val theSender = sender()
      if(!workerRegistry.contains(theSender)) {
        context.watch(theSender)
      }
      sendWork(theSender)
    case SendMoreWork =>
      sendWork(sender())
    case Terminated(theActorRef) =>
      workerRegistry.remove(theActorRef)
      if(workerRegistry.isEmpty) {
        context.system.eventStream.publish(CompleteResult())
        context.stop(self)
      }
  }

  private def sendWork(theActor: ActorRef): Unit = {
    if(perms.hasNext) {
      perms.take(batchSize).foreach(theActor ! Permutation(_))
    } else {
      workerRegistry.foreach(_ ! Kill)
    }
  }
}
