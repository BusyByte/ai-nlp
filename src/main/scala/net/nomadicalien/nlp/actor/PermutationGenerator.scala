package net.nomadicalien.nlp.actor

import akka.actor._
import akka.routing.{Broadcast, SmallestMailboxRoutingLogic, ActorRefRoutee, Router}
import net.nomadicalien.nlp.{Logging, Sentence}

/**
 * Created by Shawn on 12/16/2014.
 */
class PermutationGenerator(encryptedSentence: Sentence, comparator: ActorRef) extends Actor with Logging {
  import scala.concurrent.duration._

  val sentenceGeneratorRouter: Router = {
    val routees = Vector.fill(5) {
      val r = context.actorOf(Props(classOf[SentenceGenerator], encryptedSentence, comparator).withMailbox(mailbox))
      context watch r
      ActorRefRoutee(r)
    }
    Router(SmallestMailboxRoutingLogic(), routees)
  }

  context.system.eventStream.subscribe(self, classOf[DeadLetter])
  var numToGenerate = 5000
  var isOpen = true
  val perms = ('a' to 'z').toList.permutations
  var terminatedCount = 0
  var scheduledTask: Option[Cancellable] = None
  override def receive: Receive = {
    case Start =>
      scheduledTask = Some(context.system.scheduler.schedule(10 millis, 10 millis, self, GeneratePerms)(context.system.dispatcher))
    case GeneratePerms =>
      isOpen = true
      perms.take(numToGenerate).foreach { it =>
        sentenceGeneratorRouter.route(Permutation(it), self)
      }
      if(perms.isEmpty) {
        scheduledTask.get.cancel()
        terminatedCount = 0
        sentenceGeneratorRouter.route(Broadcast(Kill), self)
      }
    case d: DeadLetter =>
      if(isOpen) {
        numToGenerate = scala.math.max(numToGenerate - 1, 1)
        isOpen = false
      }

      d.recipient.!(d.message)(d.sender)
    case Terminated =>
      terminatedCount = terminatedCount + 1
      if(terminatedCount == 5) {
        comparator ! CompleteResult
        context.stop(self)
      }
  }
}
