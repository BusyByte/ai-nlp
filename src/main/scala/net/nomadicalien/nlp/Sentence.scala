package net.nomadicalien.nlp

import org.apache.commons.lang3.mutable.MutableDouble
import scala.collection.mutable
import net.nomadicalien.nlp.WordFrequency.WordRanking
import java.util.regex.Pattern

/**
 * User: Shawn Garner
 * Created: 4/11/13 10:49 PM
 */
class Sentence(stringToDecode : String) extends Logging {
  val NON_ALPHA_PATTERN : String = "[^\\p{Alpha}]+"
  val ALPHA_PATTERN : String = "[\\p{Alpha}]+"

  val words : List[String] = stringToDecode.toLowerCase.split(NON_ALPHA_PATTERN).toList
  val frequencyMap : Map[Char, Double] = createLetterFrequencyMap(words)
  val firstLetterFrequencyMap : Map[Char, Double] = createFirstLetterFrequencyMap(words)
  val doubleLetters : Set[Char] = createDoubleLetterSet(words)
  val letterProbabilities = calculateCharacterProbabilities(words)

  /**
   * Will swap letters without regard to case. <br>
   * Will maintain the case of the letter positions of the original sentence.
   */
  def swap(losingLetter : Char, candidateLetter : Char) : Sentence = {
    logger.debug(s"replacing ${losingLetter} with ${candidateLetter}")

    val upperCaseCharIndexes : Set[Int] = stringToDecode.zipWithIndex.filter(_._1.isUpper).map(_._2).toSet

    val substitute : Char = 26.toChar
    val newSentenceLowerCase = stringToDecode.toLowerCase.replace(candidateLetter.toLower, substitute).replace(losingLetter.toLower, candidateLetter.toLower).replace(substitute, losingLetter.toLower)
    val newSentence = newSentenceLowerCase.zipWithIndex.map { it => if(upperCaseCharIndexes.contains(it._2)) {it._1.toUpper} else {it._1} }.mkString

    new Sentence(newSentence)
  }

  private def createDoubleLetterSet(foundWordList : List[String] ) : Set[Char] = {
    val doubleLetters = mutable.Set[Char]()

    foundWordList.foreach { word =>
      var prevChar : Char = '0'
      word.foreach { theChar : Char =>
        if (theChar == prevChar){
          doubleLetters.add(theChar)
        }

        prevChar = theChar
      }
    }

    doubleLetters.toSet
  }

  private def createLetterFrequencyMap(foundWordList : List[String]) : Map[Char, Double] = {
    val letterFrequencyMap = mutable.Map[Char, MutableDouble]()

    var letterCount : Int = 0
    foundWordList.foreach {  word : String =>
      word.foreach { currentChar : Char =>
        increment(letterFrequencyMap, currentChar)
        letterCount = letterCount + 1
      }
    }

    letterFrequencyMap.values.foreach { mutableDouble : MutableDouble =>
      normalizeCount(letterCount, mutableDouble)
    }

    letterFrequencyMap.mapValues(_.doubleValue()).toMap
  }

  private def createFirstLetterFrequencyMap(foundWordList : List[String]) : Map[Char, Double] = {
    val firstLetterFrequencyMap = mutable.Map[Char, MutableDouble]()

    var letterCount : Int = 0
    foundWordList.foreach { word : String =>
      val firstChar : Char = word.charAt(0)
      increment(firstLetterFrequencyMap, firstChar)

      letterCount = letterCount + 1
    }

    firstLetterFrequencyMap.values.foreach {  mutableDouble : MutableDouble =>
      normalizeCount(letterCount, mutableDouble)
    }

    firstLetterFrequencyMap.mapValues(_.doubleValue()).toMap
  }

  private def normalizeCount(totalLetterCount : Int, observationCount : MutableDouble) {

    var doubleValue : Double = observationCount.doubleValue()
    doubleValue /= totalLetterCount
    observationCount.setValue(doubleValue)
  }

  private def increment(mapToUse : mutable.Map[Char, MutableDouble], targetChar : Char) {
    mapToUse.getOrElseUpdate(targetChar, new MutableDouble(0.0d)).increment()
  }

  override def toString() : String  = {
    stringToDecode
  }

  override def equals(obj : Any) : Boolean = {
    toString() == obj.toString
  }

  override def hashCode() : Int = {
    toString().hashCode()
  }

  def calculateCharacterProbabilities(foundWords : List[String]): Map[Char, Double] = {
    val probs = mutable.Map[Char, mutable.ListBuffer[Double]]()

    foundWords.foreach {
      word: String =>
        var priorLetter: Char = '0'
        var priorProbability: Double = 0.0d
        word.zipWithIndex.foreach {
          case (currentLetter, index) =>
            val currentProbabilityList = probs.getOrElseUpdate(currentLetter, mutable.ListBuffer[Double]())
            val newProbability: Double = {
              if (index == 0) {
                LetterFrequency.firstLetterProbabilityOf(currentLetter).getOrElse(0.0d)
              } else if (isDoubleLetter(currentLetter) && priorLetter == currentLetter) {
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

  def printWordProbabilities() {
    val sb = new StringBuilder()
    this.words.foreach {
      foundWord: String =>
        sb.append("\n")
        sb.append(foundWord)
        sb.append("\n")
        sb.append("letter prob")
        sb.append("=")
        sb.append(ProbFormatter.format(letterProbabilityCorrect(foundWord, letterProbabilities)))
        sb.append(",")
        sb.append("word prob")
        sb.append("=")
        sb.append(ProbFormatter.format(determineProbabilityWordIsCorrect(foundWord, letterProbabilities)))
        sb.append("\n")
        foundWord.foreach {
          letter: Char =>
            sb.append("[")
            sb.append(letter)
            sb.append("=")
            sb.append(ProbFormatter.format(letterProbabilities.getOrElse(letter, 0.0d)))
            sb.append("]")
        }
        sb.append("\n\n")
    }

    logger.debug(sb.toString())
  }

  def letterProbabilityCorrect(foundWord: String, probabilities: Map[Char, Double]): Double = {
    val probabilityCorrect: Double = foundWord.map {
      probabilities.getOrElse(_, 0.0d)
    }.product
    probabilityCorrect
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


  def calculateProbablilitySentenceIsCorrect(): Double = {
    val probability: Double =
      words.toSet.map {
        word: String =>
          var wordProb: Double = 1.0d
          word.foreach {
            theChar: Char =>
              wordProb *= letterProbabilities.get(theChar).getOrElse(0.0d)
          }
          wordProb
      }.product

    probability
  }

  val vowelPattern = Pattern.compile("(.*)([aAeEiIoOuUyY]+)(.*)")
  val onlyVowelPattern = Pattern.compile("[aAeEiIoOuUyY]+")

  def hasVowel(wholeDecriptedWordString: String): Boolean = {
    vowelPattern.matcher(wholeDecriptedWordString).matches()
  }

  def hasAllVowels(wholeDecriptedWordString: String): Boolean = {
    onlyVowelPattern.matcher(wholeDecriptedWordString).matches()
  }

  def isDoubleLetter(theChar : Char) = doubleLetters.contains(theChar)
}
