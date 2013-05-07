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
      val lhsPriority = calculateProbablilitySentenceIsCorrect(lhs)
      val rhsPriority = calculateProbablilitySentenceIsCorrect(rhs)

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
      val sentenceProb: Double = calculateProbablilitySentenceIsCorrect(currentSentence)
      logger.info("sentence prob correct = " + ProbFormatter.format(sentenceProb))
      if (sentenceProb > 0.6d || solutionSentence == currentSentence) {
        throw new CloseEnoughMatchException("Probability Correct is " + ProbFormatter.format(sentenceProb) + ": " + currentSentence)
      }

      val probabilities: Map[Char, Double] = calculateCharacterProbabilities(currentSentence)
      printWordProbabilities(currentSentence, probabilities)

      lastSentence = currentSentence
      visitedSentences.add(currentSentence)
      coolOff.enqueue(new SentenceCoolOff(currentSentence))

      (0 until 20).foreach {
        it =>
          val leastLikelyCorrect: Char = findLeastLikelyCorrectLetter(probabilities)
          val replacement: Char = determineMaxProbReplacement(leastLikelyCorrect, probabilities)
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

      prioritizedCandidates = prioritizedCandidates.take(10000)

    }


  }

  def calculateCharacterProbabilities(sentence: Sentence): Map[Char, Double] = {
    val probs = mutable.Map[Char, mutable.ListBuffer[Double]]()

    sentence.words.foreach {
      word: String =>
        var priorLetter: Char = '0'
        var priorProbability: Double = 0.0d
        word.zipWithIndex.foreach {
          case (currentLetter, index) =>
            val currentProbabilityList = probs.getOrElseUpdate(currentLetter, mutable.ListBuffer[Double]())
            val newProbability: Double = {
              if (index == 0) {
                LetterFrequency.firstLetterProbabilityOf(currentLetter).getOrElse(0.0d)
              } else if (sentence.isDoubleLetter(currentLetter) && priorLetter == currentLetter) {
                //TODO: this case may not be actually needed as BiGram case below may handle it
                val doubleLetterProbability: Double = LetterFrequency.doubleLetterProbabilityOf(currentLetter).getOrElse(0.0d)
                //p(c|p) = (prob (p|c) * p(c)) / p(p)
                doubleLetterProbability
              } else {
                //p(c|p) = (prob (p|c) * p(c)) / p(p)
                val probabilityOfCurrentGivenPrior: Double = BiGram.probOfAGivenB(currentLetter, priorLetter)
                probabilityOfCurrentGivenPrior
              }

            }

            currentProbabilityList += newProbability
            priorProbability = newProbability
            priorLetter = currentLetter
        }
    }

    probs.mapValues {
      list => list.sum / list.size
    }.toMap
  }


  def calculateProbablilitySentenceIsCorrect(sentence: Sentence): Double = {
    val probabilities: Map[Char, Double] = calculateCharacterProbabilities(sentence)
    val probability: Double =
      sentence.words.map {
        word: String =>
          var wordProb: Double = 1.0d
          word.foreach {
            theChar: Char =>
              wordProb *= probabilities.get(theChar).getOrElse(0.0d)
          }
          wordProb
      }.product

    probability
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

  def letterProbabilityCorrect(foundWord: String, probabilities: Map[Char, Double]): Double = {
    val probabilityCorrect: Double = foundWord.map {
      probabilities.getOrElse(_, 0.0d)
    }.product
    probabilityCorrect
  }

  val vowelPattern = Pattern.compile("(.*)([aAeEiIoOuUyY]+)(.*)")
  val onlyVowelPattern = Pattern.compile("[aAeEiIoOuUyY]+")

  def hasVowel(wholeDecriptedWordString: String): Boolean = {
    vowelPattern.matcher(wholeDecriptedWordString).matches()
  }

  def hasAllVowels(wholeDecriptedWordString: String): Boolean = {
    onlyVowelPattern.matcher(wholeDecriptedWordString).matches()
  }

  def determineProbabilityWordIsCorrect(wholeDecryptedWordString: String, probabilities: Map[Char, Double]): Double = {

    var probabilityCorrect = 0.0d
    val wordSize = wholeDecryptedWordString.length

    if (!hasVowel(wholeDecryptedWordString) || (wordSize > 1 && hasAllVowels(wholeDecryptedWordString))) {
      return probabilityCorrect
    }

    val wordRankingList: List[WordRanking] = WordFrequency.getRankingList(wordSize)

    val wordRanking: Option[WordRanking] = wordRankingList.find {
      theWordRanking: WordRanking =>
        theWordRanking.word.equalsIgnoreCase(wholeDecryptedWordString)
    }

    if (wordRanking.isDefined) {
      probabilityCorrect = wordRanking.get.probability
    } else {
      val foundWord = KnownWords.findWord(wholeDecryptedWordString)
      if (foundWord) {
        val numWordsOfSize = KnownWords.numberWordsOfSize(wordSize)
        probabilityCorrect = (1.0d / numWordsOfSize)
      } else {
        probabilityCorrect = (1.0d / WordFrequency.ESTIMATE_NUMBER_WORDS_IN_ENGLISH)
        probabilityCorrect = Math.max(probabilityCorrect, letterProbabilityCorrect(wholeDecryptedWordString, probabilities))
      }

    }

    probabilityCorrect
  }

  def printWordProbabilities(sentence: Sentence, probabilities: Map[Char, Double]) {
    val sb = new StringBuilder()
    sentence.words.foreach {
      foundWord: String =>
        sb.append("\n")
        sb.append(foundWord)
        sb.append("\n")
        sb.append("letter prob")
        sb.append("=")
        sb.append(ProbFormatter.format(letterProbabilityCorrect(foundWord, probabilities)))
        sb.append(",")
        sb.append("word prob")
        sb.append("=")
        sb.append(ProbFormatter.format(determineProbabilityWordIsCorrect(foundWord, probabilities)))
        sb.append("\n")
        foundWord.foreach {
          letter: Char =>
            sb.append("[")
            sb.append(letter)
            sb.append("=")
            sb.append(ProbFormatter.format(probabilities.getOrElse(letter, 0.0d)))
            sb.append("]")
        }
        sb.append("\n\n")
    }

    logger.debug(sb.toString())
  }


  def logTranslation(lastSentence: Sentence, currentSentence: Sentence) {
    logger.info(s"ENCRYPTED     [$stringToDecode]")
    logger.info(s"LAST DECODED  [$lastSentence]")
    logger.info(s"DECODED       [$currentSentence]")
    logger.info(s"SOLUTION      [$solution]")
  }
}
