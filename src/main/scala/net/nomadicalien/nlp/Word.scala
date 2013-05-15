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
  val theHashCode = letters.hashCode
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
            BiGram.probOfAGivenB(priorLetter, currentLetter) * LetterFrequency.probabilityOf(currentLetter).get / LetterFrequency.probabilityOf(priorLetter).get
          }
        }
      charProbStack += new CharProb(currentLetter, prob)
      priorLetter = currentLetter
    }

     charProbStack.toList
  }

  private def determineProbCorrect() = new Prob(charProbs.map(_.probability).product)


  private def determineProbabilityWordIsCorrect(): Prob = {

    val wordSize = letters.length

    if (!hasVowel || (wordSize > 1 && hasAllVowels)) {
      return new Prob()
    }
    val wordRankingList: List[WordRanking] = WordFrequency.getRankingList(wordSize)

    val wordRanking: Option[WordRanking] = wordRankingList.find {
      theWordRanking: WordRanking =>
        theWordRanking.word.equalsIgnoreCase(letters)//TODO: move to find method in WordFrequency to search by word
    }

    val probabilityCorrect : Double = {
      if (wordRanking.isDefined) {
        wordRanking.get.probability
      } else {
        val foundWord = KnownWords.findWord(letters)
        if (foundWord) {
          (1.0d / KnownWords.numberWordsOfSize(wordSize))
        } else {
          Math.max(probabilityCorrectByLetters.prob, (1.0d / WordFrequency.ESTIMATE_NUMBER_WORDS_IN_ENGLISH))
        }
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
    sb.append(probabilityCorrectByLetters.format())
    sb.append(",")
    sb.append("word prob")
    sb.append("=")
    sb.append(probabilityCorrectByWord.format())
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
    theHashCode
  }

}
