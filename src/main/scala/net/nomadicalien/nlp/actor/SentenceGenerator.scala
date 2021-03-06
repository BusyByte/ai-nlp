package net.nomadicalien.nlp.actor

import akka.actor.{Props, Actor}
import net.nomadicalien.nlp.{Logging, Sentence}
import scala.collection.mutable
import akka.pattern.pipe

import scala.concurrent.Future
import scala.util.Random

class SentenceGenerator(encryptedSentence: Sentence) extends Actor with Logging {
  val comparator = context.actorOf(Props[SentenceComparator])
  val generator = context.system.actorSelection("/user/PermutationGenerator")
  val random = new Random()

  var working = false

  val workQueue = mutable.Queue[List[Char]]()


  override def preStart(): Unit = generator.tell(RegisterWorker, self)

  override def receive: Actor.Receive = {
    case Permutation(perm) =>
      if(random.nextInt(1000000) == 0) {
        logger.info(s"Current Perm [${perm.mkString}]")
      }
      if(working) {
        workQueue.enqueue(perm)
      } else {
        working = true
        doWork(perm)
      }
    case WorkDone =>
      if(workQueue.isEmpty) {
        working = false
        generator ! SendMoreWork
      } else {
        doWork(workQueue.dequeue())
      }
  }

  def doWork(perm: List[Char]): Unit = {
    implicit val ec = context.dispatcher
    Future {
      val zipped = encryptedSentence.distinctLetters zip perm
      comparator ! encryptedSentence.swapMultiple(zipped.toMap)
      WorkDone
    } pipeTo self
  }

}
