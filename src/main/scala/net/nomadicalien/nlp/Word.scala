package net.nomadicalien.nlp

import java.util.regex.Pattern

import cats.Show
import net.nomadicalien.nlp.Probability.Probability
import net.nomadicalien.nlp.WordFrequency.WordRanking

object Word {
  val onlyVowelPattern = Pattern.compile("[aAeEiIoOuUyY]+")
  val onlyConsonantsPattern = Pattern.compile("[bBcCdDfFgGhHjJkKlLmMnNpPqQrRsStTvVxXzZwWyY]+")

  implicit def wordShow(implicit p: Show[Probability]) = new Show[Word] {
    override def show(word: Word): String = {
      val sb = new StringBuilder()
      sb.append("\n")
      sb.append(word)
      sb.append("\n")
      sb.append("letter prob")
      sb.append("=")
      sb.append(p.show(word.probabilityCorrectByLetters))
      sb.append(",")
      sb.append("word prob")
      sb.append("=")
      sb.append(p.show(word.probabilityCorrectByWord))
      sb.append("\n\n")

      sb.toString()
    }

  }
}

case class Word(letters: String) {

  lazy val hasAllVowels =  Word.onlyVowelPattern.matcher(letters).matches()

  lazy val hasAllConsonants = Word.onlyConsonantsPattern.matcher(letters).matches()

  lazy val probabilityCorrectByLetters: Probability = determineProbCorrect(determineCharProbs())

  lazy val probabilityCorrectByWord: Probability = determineProbabilityWordIsCorrect()

  private def determineCharProbs(): List[LetterProb] = {
    val (_, reverseCharProbList) = letters.foldLeft((None: Option[Char], List[LetterProb]()))(
      (accumulator, currentLetter) => {
        val priorLetterMaybe: Option[Char] = accumulator._1
        val currentList = accumulator._2
        val prob: Double = {
          if (priorLetterMaybe.isEmpty) {
            LetterFrequency.firstLetterProbabilityOf(currentLetter).getOrElse(0.0d)
          } else {
            val priorLetter = priorLetterMaybe.get
            if (currentLetter == priorLetter) {
              LetterFrequency.doubleLetterProbabilityOf(currentLetter).getOrElse(0.0d)
            } else {
              BiGram.probOfAGivenB(currentLetter, priorLetter)
            }
          }
        }

        (Some(currentLetter), LetterProb(currentLetter, prob) :: currentList)
      }
    )

    reverseCharProbList // probabilities are reverse order but should not matter since we only multiply them
  }

  private def determineProbCorrect(charProbs: List[LetterProb]):Probability = charProbs.map(_.probability).product

  private def determineProbabilityWordIsCorrect(): Probability = {

    val wordSize = letters.length

    if (hasAllConsonants || (wordSize > 1 && hasAllVowels)) {
      return 0.0d
    }
    val wordRanking: Option[WordRanking] = WordFrequency.getWordRanking(letters)

    val probabilityCorrect : Double = {
      if (wordRanking.isDefined) {
        wordRanking.get.probability
      } else {
        val foundWord = KnownWords.findWord(letters)
        if (foundWord) {
          1.0d / KnownWords.numberWordsOfSize(wordSize)
        } else {
          probabilityCorrectByLetters
        }
      }
    }

    probabilityCorrect
  }

  override def toString: String = letters

}
