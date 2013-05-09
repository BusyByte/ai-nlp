package net.nomadicalien.nlp

import scala.collection.mutable
import net.nomadicalien.nlp.WordFrequency.WordRanking
import java.util.regex.Pattern

object Word {
  val vowelPattern = Pattern.compile("(.*)([aAeEiIoOuUyY]+)(.*)")
  val onlyVowelPattern = Pattern.compile("[aAeEiIoOuUyY]+")

}

/**
 * User: Shawn Garner
 * Created: 5/8/13 8:26 PM
 */
class Word(val letters : String) {

  val hasVowel  = Word.vowelPattern.matcher(letters).matches()
  val hasAllVowels =  Word.onlyVowelPattern.matcher(letters).matches()

  val charProbs = determineCharProbs()
  val probabilityCorrectByLetters = determineProbCorrect()
  val probabilityCorrectByWord = determineProbabilityWordIsCorrect()

  private def determineCharProbs() : List[CharProb] = {
    val charProbStack = new mutable.ListBuffer[CharProb]()
    var priorLetter : Char = '0'
    letters.zipWithIndex.foreach {
      case (currentLetter, index) =>
        val prob : Double = {
          if(index == 0) {
            LetterFrequency.firstLetterProbabilityOf(currentLetter).getOrElse(0.0d)
          } else if (currentLetter == priorLetter) {
            LetterFrequency.doubleLetterProbabilityOf(currentLetter).getOrElse(0.0d)
          }  else {
            BiGram.probOfAGivenB(currentLetter, priorLetter)
          }
        }
      charProbStack += new CharProb(currentLetter, prob)
      priorLetter = currentLetter
    }

     charProbStack.toList
  }

  private def determineProbCorrect() = new Prob(charProbs.map(_.probability).product)

  private def determineProbabilityWordIsCorrect(): Prob = {

    var probabilityCorrect = 0.0d
    val wordSize = letters.length

    if (!hasVowel || (wordSize > 1 && hasAllVowels)) {
      return new Prob()
    }

    val wordRankingList: List[WordRanking] = WordFrequency.getRankingList(wordSize)

    val wordRanking: Option[WordRanking] = wordRankingList.find {
      theWordRanking: WordRanking =>
        theWordRanking.word.equalsIgnoreCase(letters)
    }

    if (wordRanking.isDefined) {
      probabilityCorrect = wordRanking.get.probability
    } else {
      val foundWord = KnownWords.findWord(letters)
      if (foundWord) {
        val numWordsOfSize = KnownWords.numberWordsOfSize(wordSize)
        probabilityCorrect = (1.0d / numWordsOfSize)
      } else {
        probabilityCorrect = (1.0d / WordFrequency.ESTIMATE_NUMBER_WORDS_IN_ENGLISH)
        probabilityCorrect = Math.max(probabilityCorrect, probabilityCorrect)
      }

    }
    new Prob(probabilityCorrect)
  }


  def format() : String = {
    val sb = new StringBuilder()
    sb.append("\n")
    sb.append(this.toString())
    sb.append("\n")
    sb.append("letter prob")
    sb.append("=")
    sb.append(probabilityCorrectByLetters)
    sb.append(",")
    sb.append("word prob")
    sb.append("=")
    sb.append(probabilityCorrectByWord)
    sb.append("\n")
    this.charProbs.foreach {
      charProb: CharProb =>
        sb.append("[")
        sb.append(charProb.letter)
        sb.append("=")
        sb.append(ProbFormatter.format(charProb.probability))
        sb.append("]")
    }
    sb.append("\n\n")

    sb.toString()
  }


  override def toString() : String  = {
    letters
  }

  override def equals(obj : Any) : Boolean = {
    letters == obj.toString
  }

  override def hashCode() : Int = {
    letters.hashCode()
  }

}
