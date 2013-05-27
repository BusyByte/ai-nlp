package net.nomadicalien.nlp

import scala.collection.mutable
import scala.Double
import java.util.Collections

/**
 * User: Shawn Garner
 * Created: 4/16/13 10:22 PM
 */
class NLP2(stringToDecode: String, solution: String) extends Randomness with Logging {

  def sentenceOrdering = new Ordering[Sentence] {
    def compare(lhs: Sentence, rhs: Sentence): Int = {
      val lhsPriority = lhs.probabilityCorrect
      val rhsPriority = rhs.probabilityCorrect

      if (lhsPriority < rhsPriority) {
        -1
      }
      else if (lhsPriority > rhsPriority) {
        1
      }
      else {
        0
      }
    }

  }

  class SentenceCoolOff(val sentence : Sentence) {
    var coolOff : Int = 10
    def cool = {coolOff = coolOff - 1}
    def cooledOff : Boolean = {coolOff <= 0}
  }

  def process {
    val solutionSentence = new Sentence(solution)
    val visitedSentences = mutable.WeakHashMap[Sentence, Boolean]()
    var prioritizedCandidates = mutable.PriorityQueue[Sentence]()(sentenceOrdering)

    prioritizedCandidates.enqueue(new Sentence(stringToDecode))
    var lastSentence: Sentence = null

    while (prioritizedCandidates.nonEmpty) {
      logger.debug("NumVisitedSentences=" + visitedSentences.size)
      logger.debug("NumCandidates=" + prioritizedCandidates.size)

      val currentSentence = prioritizedCandidates.dequeue()

      logTranslation(lastSentence, currentSentence)
      val sentenceProb: Prob = currentSentence.probabilityCorrect
      logger.info("sentence prob correct = " + sentenceProb.prob)
      if (sentenceProb.prob > 0.60d || solutionSentence == currentSentence) {
        throw new CloseEnoughMatchException("Probability Correct is " + sentenceProb.format() + ": " + currentSentence)
      }

      currentSentence.printWordProbabilities()

      lastSentence = currentSentence
      visitedSentences.put(currentSentence, true)

      val leastLikelyWord = currentSentence.findLeastLikelyWord.toString()
      logger.debug(s"least likely word [$leastLikelyWord]")
      val allCandidateReplacements = ('a' to 'z').toSet
      (0 until 26).foreach { it =>
          val replacee: Char = leastLikelyWord.charAt(nextInt(leastLikelyWord.size))
          val subsetOfReplacements = (allCandidateReplacements - replacee).toList
          val replacement: Char = subsetOfReplacements(nextInt(subsetOfReplacements.size))
          val newSentence = currentSentence.swap(replacee, replacement)
          if (!visitedSentences.contains(newSentence) && !prioritizedCandidates.exists(_ == newSentence)) {
            prioritizedCandidates.enqueue(newSentence)
          }
      }

      if(prioritizedCandidates.size > 8000) {
        prioritizedCandidates = prioritizedCandidates.take(4000)
      }
    }
  }

  def logTranslation(lastSentence: Sentence, currentSentence: Sentence) {
    logger.info(s"ENCRYPTED     [$stringToDecode]")
    logger.info(s"LAST DECODED  [$lastSentence]")
    logger.info(s"DECODED       [$currentSentence]")
    logger.info(s"SOLUTION      [$solution]")
  }
}
