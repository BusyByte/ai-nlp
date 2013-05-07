package net.nomadicalien.nlp

import scala.collection.mutable
import scala.Double
import java.util.regex.Pattern
import net.nomadicalien.nlp.WordFrequency.WordRanking

/**
 * User: Shawn Garner
 * Created: 4/16/13 10:22 PM
 */
class NLP2(stringToDecode: String, solution: String) extends Randomness with Logging {

  def sentenceOrdering = new Ordering[Sentence] {
    def compare(lhs: Sentence, rhs: Sentence): Int = {
      val lhsPriority = lhs.calculateProbablilitySentenceIsCorrect()
      val rhsPriority = rhs.calculateProbablilitySentenceIsCorrect()

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
    val coolOff = mutable.Queue[SentenceCoolOff]()
    val visitedSentences = mutable.Set[Sentence]()
    var prioritizedCandidates = mutable.PriorityQueue[Sentence]()(sentenceOrdering)

    prioritizedCandidates.enqueue(new Sentence(stringToDecode))
    var lastSentence: Sentence = null

    while (prioritizedCandidates.nonEmpty) {
      logger.debug("CoolOffSize=" + coolOff.size)
      logger.debug("NumVisitedSentences=" + visitedSentences.size)
      logger.debug("NumCandidates=" + prioritizedCandidates.size)

      val currentSentence = prioritizedCandidates.dequeue()

      logTranslation(lastSentence, currentSentence)
      val sentenceProb: Double = currentSentence.calculateProbablilitySentenceIsCorrect()
      logger.info("sentence prob correct = " + ProbFormatter.format(sentenceProb))
      if (sentenceProb > 0.6d || solutionSentence == currentSentence) {
        throw new CloseEnoughMatchException("Probability Correct is " + ProbFormatter.format(sentenceProb) + ": " + currentSentence)
      }

      currentSentence.printWordProbabilities()

      lastSentence = currentSentence
      visitedSentences.add(currentSentence)
      coolOff.enqueue(new SentenceCoolOff(currentSentence))

      (0 until 20).foreach {
        it =>
          val leastLikelyCorrect: Char = findLeastLikelyCorrectLetter(currentSentence.letterProbabilities)
          val replacement: Char = determineMaxProbReplacement(leastLikelyCorrect, currentSentence.letterProbabilities)
          val newSentence = currentSentence.swap(leastLikelyCorrect, replacement)
          if (!visitedSentences.contains(newSentence)) {
            prioritizedCandidates.enqueue(newSentence)
          }
      }

      val topOfStack = coolOff.front
      topOfStack.cool
      if(topOfStack.cooledOff) {
        prioritizedCandidates.enqueue(coolOff.dequeue().sentence)
      }

      prioritizedCandidates = prioritizedCandidates.take(100000)

    }


  }


  def determineMaxProbReplacement(leastLikelyCorrect: Char, probabilities: Map[Char, Double]): Char = {
    val allReplacementCandidates = mutable.Set[Char]()

    val sortedByProb = probabilities.toList.sortBy {
      case (theChar: Char, prob: Double) => 1.0d - prob
    }
    val numLetters = sortedByProb.size

    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)

    allReplacementCandidates.add(sortedByProb(nextInt((0.35d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.35d * numLetters).toInt))._1)

    allReplacementCandidates.add(sortedByProb(nextInt(numLetters))._1)

    val lettersNotInPhrase = ('a' to 'z').toSet.--(probabilities.keySet)
    allReplacementCandidates.++=(lettersNotInPhrase)
    allReplacementCandidates.-=(leastLikelyCorrect)
    val allReplacementCandidateList = allReplacementCandidates.toList
    allReplacementCandidateList(nextInt(allReplacementCandidateList.size))
  }


  def findLeastLikelyCorrectLetter(probabilities: Map[Char, Double]): Char = {
    val sortedEntries: List[Char] = probabilities.toList.sortBy(_._2).map(_._1).toList
    sortedEntries(nextInt(3))
  }

  def logTranslation(lastSentence: Sentence, currentSentence: Sentence) {
    logger.info(s"ENCRYPTED     [$stringToDecode]")
    logger.info(s"LAST DECODED  [$lastSentence]")
    logger.info(s"DECODED       [$currentSentence]")
    logger.info(s"SOLUTION      [$solution]")
  }
}
