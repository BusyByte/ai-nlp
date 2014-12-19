package net.nomadicalien.nlp.actor

import akka.actor._
import akka.routing.{Broadcast, SmallestMailboxRoutingLogic, ActorRefRoutee, Router}
import net.nomadicalien.nlp.{Logging, Sentence}

/**
 * Created by Shawn on 12/16/2014.
 */
class PermutationGenerator(encryptedSentence: Sentence) extends Actor with Logging {
  val comparator = context.system.actorSelection("/user/SentenceComparator")

  context.system.eventStream.subscribe(self, classOf[SlowDown])

  val sentenceGeneratorRouter: Router = {
    val routees = Vector.fill(numWorkers) {
      val r = context.actorOf(Props(classOf[SentenceGenerator], encryptedSentence).withMailbox(mailbox))
      context watch r
      ActorRefRoutee(r)
    }
    Router(SmallestMailboxRoutingLogic(), routees)
  }

  var numToGenerate = 15000
  val perms = ('a' to 'z').toList.permutations
  var terminatedCount = 0
  var scheduledTask: Option[Cancellable] = None
  override def receive: Receive = {
    case Start =>
      scheduledTask = Some(context.system.scheduler.schedule(generationSchedule, generationSchedule, self, GeneratePerms)(context.system.dispatcher))
    case GeneratePerms =>
      perms.take(numToGenerate).foreach { it =>
        sentenceGeneratorRouter.route(Permutation(it), self)
      }
      if(perms.isEmpty) {
        scheduledTask.get.cancel()
        sentenceGeneratorRouter.route(Broadcast(Kill), self)
      }
    case SlowDown => numToGenerate = scala.math.max(numToGenerate - 1, 1)
    case Terminated =>
      terminatedCount = terminatedCount + 1
      if(terminatedCount == numWorkers) {
        comparator ! CompleteResult
        context.stop(self)
      }
  }
}
