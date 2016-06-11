package net.nomadicalien.nlp.actor.stream.genetic

import java.util.concurrent.TimeUnit

import akka.stream.actor.ActorSubscriberMessage.OnNext
import akka.stream.actor.{MaxInFlightRequestStrategy, ActorSubscriber}
import net.nomadicalien.nlp.actor.ReplacePool
import net.nomadicalien.nlp.{ProbFormatter, Sentence, Logging}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/**
  * Created by Shawn on 6/1/2016.
  */
class GeneticSelectionActor(val solutionSentence: Sentence) extends ActorSubscriber with Logging{
  import scala.collection.mutable
  val rng = new Random()

  implicit val ordering: Ordering[Sentence] = new Ordering[Sentence] {
    override def compare(x: Sentence, y: Sentence): Int = Ordering.Double.compare(x.probabilityCorrect, y.probabilityCorrect)
  }

  val bestMatchQueue = new mutable.PriorityQueue[Sentence]

  override val requestStrategy = new MaxInFlightRequestStrategy(max = 10) {
    override def inFlightInternally: Int = 0
  }

  def receive = {
    case OnNext(s: Sentence) =>
      if(rng.nextInt(100000) == 0) {
        logSentence("SANITY CHECK", s)
      }

      if(!bestMatchQueue.exists(s2 => s2 == s2)) {
        bestMatchQueue.enqueue(s)
        if(s == solutionSentence) {
          logSentence("***FOUND IT***", s)
          logger.info("!!shutting down!!!")
          implicit val ec: ExecutionContext = context.system.dispatcher
          context.system.scheduler.scheduleOnce(FiniteDuration(2l, TimeUnit.SECONDS), new Runnable {
            def run() = {context.system.terminate()}
          })

        }
      }


      if(bestMatchQueue.size > 10000) {
        logSentence("BEST", bestMatchQueue.head)
        val best = Stream.continually {
          bestMatchQueue.dequeue()
        }.take(100).toList
        bestMatchQueue.clear()
        bestMatchQueue.enqueue(best :_*)
        context.system.eventStream.publish(ReplacePool(best))
      }
  }

  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }
}
