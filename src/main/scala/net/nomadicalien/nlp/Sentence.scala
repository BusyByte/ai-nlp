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
    String.format(stringToDecode.replaceAll(ALPHA_PATTERN, "%s"), words.toArray)
  }

  override def equals(obj : Any) : Boolean = {
    toString().equals(obj.toString)
  }

  override def hashCode() : Int = {
    toString().hashCode()
  }


  def logTranslation(lastSentenceDecoded : String) {
    logger.info("ENCRYPTED[$stringToDecode]")
    if (lastSentenceDecoded != null) {
      logger.info("LASTDECOD[$lastSentenceDecoded]")
    }
    logger.info("  DECODED[${this.toString()}]")
    logger.info(" SOLUTION[$SOLUTION]")
  }

  val SOLUTION = "The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.".toLowerCase()

  def matches() : Boolean = {
    SOLUTION == this.toString()
  }


  /*void swap(val CharWithProbability originalCharProbability, val CharWithProbability replacementCharProbability) {
    val originalChar = originalCharProbability.getCharText()
    val originalLetter = this.CharToLetterMap.get(originalChar)

    val replacementChar = replacementCharProbability.getCharText()
    val replacementLetter = this.CharToLetterMap.get(replacementChar)

    log.debug(String.format("Replacing %s with %s", originalChar, replacementChar))

    swapQuiet(originalCharProbability, replacementCharProbability)

    val event = new Event()
    originalLetter.fireCharChanged(event)
    replacementLetter?.fireCharChanged(event)
  }
*/
  /*public void calculateProbabilities() {
    val CharSet = new HashSet[Char](37)
        words.each { word -]
            val letter = word.letters.get(0).letter
            val charText = letter.letter
            if (!CharSet.contains(charText)) {
                CharSet.add(charText)
                letter.fireCharChanged(new Event())//forces the rest of Chars to recalc because this is prior
            }
        }
  }
*/
  /*public void swapQuiet(val CharWithProbability originalCharProbability, val CharWithProbability replacementCharProbability) {
      val originalChar = originalCharProbability.getCharText()
      val originalProbability = originalCharProbability.getProbability()

      val replacementChar = replacementCharProbability.getCharText()
      val replacementProbability = replacementCharProbability.getProbability()

      val originalLetter = this.CharToLetterMap.remove(originalChar)
      val replacementLetter = this.CharToLetterMap.remove(replacementChar)

      originalLetter.setLetter(replacementChar)
      originalLetter.setProbabilityCorrect(replacementProbability)

      if (replacementLetter) {
          replacementLetter.letter = originalChar
          replacementLetter.probabilityCorrect = originalProbability
          this.CharToLetterMap.put(replacementLetter.letter, replacementLetter)
      }

      this.CharToLetterMap.put(originalLetter.letter, originalLetter)

  }*/

  /*public Double probabilityCorrect() {
      var probability = 0.0d
      words.each { word-]
          probability += word.letterProbabilityCorrect()
      }
      probability /= this.words.size()

      return probability
  }*/

  def isDoubleLetter(theChar : Char) = doubleLetters.contains(theChar)
}
