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
  lazy val probabilityCorrectByLetters = determineProbCorrect(determineCharProbs())

  private def determineCharProbs(): List[CharProb] = {
    val (_, reverseCharProbList) = letters.foldLeft((None: Option[Char], List[CharProb]()))(
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
              BiGram.biGramLookup(currentLetter + "|" + priorLetter)
            }
          }
        }

        (Some(currentLetter), CharProb(currentLetter, prob) :: currentList)
      }
    )

    reverseCharProbList // probabilities are reverse order but should not matter since we only multiply them
  }

  private def determineProbCorrect(charProbs: List[CharProb]) = new Prob(charProbs.map(_.probability).product)


  def format(): String = {
    val sb = new StringBuilder()
    sb.append("\n")
    sb.append(this.toString())
    sb.append("\n")
    sb.append("letter prob")
    sb.append("=")
    sb.append(probabilityCorrectByLetters.format())
    sb.append("\n\n")

    sb.toString()
  }

  override def toString: String = letters

}
