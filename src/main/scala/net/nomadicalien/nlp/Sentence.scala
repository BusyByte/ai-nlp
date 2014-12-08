package net.nomadicalien.nlp


object Sentence {
  val substituteChar : Char = 26.toChar
  val NON_ALPHA_PATTERN : String = "[^\\p{Alpha}]+"
  val ALPHA_PATTERN : String = "[\\p{Alpha}]+"
  val PUNCTUATION: String = "\\p{Punct}"

}

object SentenceOrdering extends Ordering[Sentence]  {
    def compare(lhs: Sentence, rhs: Sentence): Int = {
      val lhsPriority = lhs.probabilityCorrect
      val rhsPriority = rhs.probabilityCorrect

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

/**
 * User: Shawn Garner
 * Created: 4/11/13 10:49 PM
 */
case class Sentence(stringToDecode : String) extends Logging {
  val encodedString = stringToDecode.toLowerCase.replaceAll(Sentence.PUNCTUATION, "")

  val words : List[Word] = encodedString.split(Sentence.NON_ALPHA_PATTERN).map(new Word(_)).toList

  val probabilityCorrect : Prob = determineProbabilityCorrect()

  val distinctLetters = encodedString.replaceAll(Sentence.NON_ALPHA_PATTERN, "").foldRight(List[Char]()) (
    (theChar: Char, theList: List[Char]) => theList.contains(theChar) match {
      case true => theList
      case _ => theChar :: theList
    }
  ) map(new LowerCaseLetter(_))

  /**
   * Will swap letters without regard to case.
   */
  def swap(losingLetter: LowerCaseLetter, candidateLetter: LowerCaseLetter): Sentence = Sentence(swapString(encodedString, losingLetter, candidateLetter))

  def swapMultiple(swaps: List[(LowerCaseLetter, LowerCaseLetter)]): Sentence = {
    Sentence(
      swaps.foldLeft(encodedString) {
      (acc, swapLetter) =>
        val losingLetter = swapLetter._1
        val candidateLetter = swapLetter._2
        swapString(acc, losingLetter, candidateLetter)
      }
    )
  }

  //TODO: could return input if loosingLetter and candidate letter match
  private def swapString(input: String, losingLetter: LowerCaseLetter, candidateLetter: LowerCaseLetter): String = {
    val loosingChar: Char = losingLetter.lowerCaseChar
    val candidateChar: Char = candidateLetter.lowerCaseChar
    if(loosingChar == candidateChar)
      input
    else
      input.replace(candidateChar, Sentence.substituteChar).replace(loosingChar, candidateChar).replace(Sentence.substituteChar, loosingChar)
  }

  def findLeastLikelyWord() : Word = {
    words.sortWith {(lhs, rhs)=> lhs.probabilityCorrectByLetters < rhs.probabilityCorrectByLetters}.head
  }

  override def toString: String  = encodedString

  def printWordProbabilities() = {
    this.words.map(_.format()).mkString
  }

  private def determineProbabilityCorrect(): Prob = {
    val probability: Double =
      words.map(_.probabilityCorrectByLetters.prob).sum / words.size

    new Prob(probability)
  }

}
