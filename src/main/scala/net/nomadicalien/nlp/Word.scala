package net.nomadicalien.nlp

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
  lazy val probabilityCorrectByLetters: Probability = determineProbCorrect(determineCharProbs())

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
              BiGram.biGramLookup(currentLetter + "|" + priorLetter)
            }
          }
        }

        (Some(currentLetter), LetterProb(currentLetter, prob) :: currentList)
      }
    )

    reverseCharProbList // probabilities are reverse order but should not matter since we only multiply them
  }

  private def determineProbCorrect(charProbs: List[LetterProb]):Probability = charProbs.map(_.probability).product


  def format(): String = {
    val sb = new StringBuilder()
    sb.append("\n")
    sb.append(this.toString())
    sb.append("\n")
    sb.append("letter prob")
    sb.append("=")
    sb.append(ProbFormatter.format(probabilityCorrectByLetters))
    sb.append("\n\n")

    sb.toString()
  }

  override def toString: String = letters

}
