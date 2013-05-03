package net.nomadicalien.nlp

import org.apache.commons.lang3.mutable.MutableDouble
import scala.collection.mutable

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
    toString().equals(obj.toString)
  }

  override def hashCode() : Int = {
    toString().hashCode()
  }


  def logTranslation(lastSentenceDecoded : String) {
    logger.info(s"ENCRYPTED\t\t[$stringToDecode]")
    if (lastSentenceDecoded != null) {
      logger.info(s"LAST DECODED\t[$lastSentenceDecoded]")
    }
    logger.info(s"DECODED\t\t[${this.toString()}]")
    logger.info(s"SOLUTION\t\t[$SOLUTION]")
  }

  val SOLUTION = "The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year."

  def matches() : Boolean = {
    SOLUTION == this.toString()
  }

  def isDoubleLetter(theChar : Char) = doubleLetters.contains(theChar)
}
