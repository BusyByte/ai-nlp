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
case class Word(letters: String) {
  //def hasVowel  = Word.vowelPattern.matcher(letters).matches()
  //def hasAllVowels =  Word.onlyVowelPattern.matcher(letters).matches()
  val probabilityCorrectByLetters = determineProbCorrect(determineCharProbs())

  //val probabilityCorrectByWord = determineProbabilityWordIsCorrect()

  private def determineCharProbs() : List[CharProb] = {
    val (_,reverseCharProbList) = letters.zipWithIndex.foldLeft(('0', List[CharProb]())) (
      (accumulator, currentElement) => {
        val currentLetter = currentElement._1
        val index = currentElement._2
        val priorLetter = accumulator._1
        val currentList = accumulator._2
        val prob: Double = {
          if (index == 0) {
            LetterFrequency.firstLetterProbabilityOf(currentLetter).getOrElse(0.0d)
          } else if (currentLetter == priorLetter) {
            LetterFrequency.doubleLetterProbabilityOf(currentLetter).getOrElse(0.0d)
          } else {
            BiGram.biGramLookup(s"$currentLetter|$priorLetter")
          }
        }

        (currentLetter, CharProb(currentLetter, prob) :: currentList)
      }
    )

    reverseCharProbList.reverse
  }

  private def determineProbCorrect(charProbs: List[CharProb]) = new Prob(charProbs.map(_.probability).product)


 /* private def determineProbabilityWordIsCorrect(): Prob = {

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
          1.0d / KnownWords.numberWordsOfSize(wordSize)
        } else {
          Math.max(probabilityCorrectByLetters.prob, 1.0d / WordFrequency.ESTIMATE_NUMBER_WORDS_IN_ENGLISH)
        }
      }
    }

    new Prob(probabilityCorrect)
  }*/


  def format() : String = {
    val sb = new StringBuilder()
    sb.append("\n")
    sb.append(this.toString())
    sb.append("\n")
    sb.append("letter prob")
    sb.append("=")
    sb.append(probabilityCorrectByLetters.format())
   /* sb.append(",")
    sb.append("word prob")
    sb.append("=")
    sb.append(probabilityCorrectByWord.format())*/
/*
    sb.append("\n")
    this.charProbs.foreach {
      charProb: CharProb =>
        sb.append("[")
        sb.append(charProb.letter)
        sb.append("=")
        sb.append(ProbFormatter.format(charProb.probability))
        sb.append("]")
    }
*/
    sb.append("\n\n")

    sb.toString()
  }

  override def toString: String  =  letters

}
