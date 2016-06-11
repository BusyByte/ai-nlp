package net.nomadicalien.nlp.actor.stream.genetic

import java.security.SecureRandom

import akka.stream.actor.ActorPublisher
import akka.stream.actor.ActorPublisherMessage.{Request, Cancel}
import net.nomadicalien.nlp.{Letter, Logging, Sentence}
import net.nomadicalien.nlp.actor.ReplacePool

import scala.annotation.tailrec
import scala.util.Random

class ActorSentencePermSource(val encryptedSentence: Sentence) extends ActorPublisher[Sentence] with Logging {

  context.system.eventStream.subscribe(self, classOf[ReplacePool])

  val rng = new Random(new SecureRandom())
  var pool: Vector[Sentence] = Vector(encryptedSentence)

  val letterPool: Array[Letter] = ('a' to 'z').toArray
  val numLetters = 26
  @tailrec
  final def sampleIndexedPool(n: Int, poolSize: Int, samples: List[Int] = List.empty[Int]): List[Int] = {
    if(n == samples.size) {
      samples
    } else {
      sampleIndexedPool(n, poolSize, rng.nextInt(poolSize) :: samples)
    }
  }

  def randomSample(base: Sentence): Sentence = {
    val letters = base.distinctLetters
    sampleDistinctLetters(letters, base)
  }

  @tailrec
  final def sampleLetters(sampleSize: Int, letters:List[Letter], samples: List[Letter] = List.empty[Letter]):List[Letter] = {
    if(samples.size == sampleSize || samples.size == letters.size) {
      samples
    } else {
      val nextSampleNum = rng.nextInt(letters.size)
      val sample = letters(nextSampleNum)
      val remainingLetters = letters.filterNot(l => l == sample)
      sampleLetters(sampleSize, remainingLetters, sample::samples)
    }
  }

  def sampleDistinctLetters(distinctLetters: List[Letter], base: Sentence) = {
    val numDistinctLetters = distinctLetters.size
    val numReplacements: Int = rng.nextInt(math.max(numDistinctLetters/2, 1)) + 1
    val losingLetters = sampleLetters(numReplacements, distinctLetters)
    val replacementPool = letterPool.filterNot(losingLetters.contains).toList
    val replacementLetters = sampleLetters(losingLetters.size, replacementPool)


    val swaps = Array.fill(26)(Option.empty[Char])
    (losingLetters zip replacementLetters).foreach {p =>
      val ll = p._1
      val rl = p._2
      val llIndex = ll - 'a'
      swaps.update(llIndex, Some(rl))
    }
    base.swapMultipleA(swaps)
  }

  def leastLikelyWordSample(base: Sentence): Sentence = {
    val letters = base.findLeastLikelyWord().letters.toList.distinct
    sampleDistinctLetters(letters, base)
  }



  def receive = {
    case Request(_) =>
      deliverBuf()
    case Cancel =>
      context.stop(self)
    case ReplacePool(newPool) =>
      pool = newPool
  }

  @tailrec
  final def deliverBuf(): Unit = {
    if (totalDemand > 0) {
      if (totalDemand <= Int.MaxValue) {
        fullFillDemand(totalDemand.toInt)
      } else {
        fullFillDemand(Int.MaxValue)
        deliverBuf()
      }
    }
  }

  def fullFillDemand(theDemand: Int): Unit = {
    val groupedSampleTypes = chooseSampleTypes(theDemand).groupBy(s =>s)

    val randomPooledSamples = groupedSampleTypes.getOrElse(RandomPooledSample, List.empty[SamplingType])

    sampleIndexedPool(randomPooledSamples.size, pool.size)
      .map(pool.apply)
      .map(randomSample)
      .foreach(onNext)

    val leastLikelyWordPooledSamples = groupedSampleTypes.getOrElse(LeastLikelyWordPooledSample, List.empty[SamplingType])
    sampleIndexedPool(leastLikelyWordPooledSamples.size, pool.size)
      .map(pool.apply)
      .map(leastLikelyWordSample)
      .foreach(onNext)
  }

  sealed trait SamplingType
  case object RandomPooledSample extends SamplingType
  case object LeastLikelyWordPooledSample extends SamplingType

  def chooseSampleTypes(n: Int): Seq[SamplingType] = {
    Stream.continually {
      val sampleTypeNumber = rng.nextInt(100)
      if(sampleTypeNumber < 10) {
        LeastLikelyWordPooledSample
      } else {
        RandomPooledSample
      }
    }.take(n)
  }

}
