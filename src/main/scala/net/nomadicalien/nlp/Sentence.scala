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
  ) map LowerCaseLetter

  /**
   * Will swap letters without regard to case.
   */
  def swap(losingLetter: LowerCaseLetter, candidateLetter: LowerCaseLetter) : Sentence = {
    //logger.debug(s"replacing $losingLetter with $candidateLetter")

    val newSentence = encodedString.replace(candidateLetter.toChar, Sentence.substituteChar).replace(losingLetter.toChar, candidateLetter.toChar).replace(Sentence.substituteChar, losingLetter.toChar)

    new Sentence(newSentence)
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
