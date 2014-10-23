package net.nomadicalien.nlp


object Sentence {
  val substituteChar : Char = 26.toChar
  val NON_ALPHA_PATTERN : String = "[^\\p{Alpha}]+"
  val ALPHA_PATTERN : String = "[\\p{Alpha}]+"

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
class Sentence(stringToDecode : String) extends Logging {
  val encodedString = stringToDecode.toLowerCase.replaceAll("\\p{Punct}", "")

  val words : List[Word] = encodedString.split(Sentence.NON_ALPHA_PATTERN).map(new Word(_)).toList

  val probabilityCorrect : Prob = determineProbabilityCorrect()

  val distinctLetters = encodedString.foldLeft(List[Char]()) (
    (theList: List[Char], theChar: Char) => theList.contains(theChar) match {
      case true => theList
      case _ => theList :+ theChar
    }
  ) map (LowerCaseLetter(_))

  /**
   * Will swap letters without regard to case.
   */
  def swap(losingLetter: LowerCaseLetter, candidateLetter: LowerCaseLetter) : Sentence = {
    logger.debug(s"replacing $losingLetter with $candidateLetter")

    val newSentence = encodedString.replace(candidateLetter.toChar, Sentence.substituteChar).replace(losingLetter.toChar, candidateLetter.toChar).replace(Sentence.substituteChar, losingLetter.toChar)

    new Sentence(newSentence)
  }

  def findLeastLikelyWord() : Word = {
    words.sortWith {(lhs, rhs)=> lhs.probabilityCorrectByLetters < rhs.probabilityCorrectByLetters}.head
  }

  override def toString: String  = encodedString

  override def equals(obj : Any) : Boolean = {
    Option(obj) match {
      case Some(rhs) => encodedString == obj.toString
      case _ => false
    }
  }

  override def hashCode() : Int = encodedString.hashCode


  def printWordProbabilities() = {
    this.words.map(_.format()).mkString
  }

  private def determineProbabilityCorrect(): Prob = {
    val probability: Double =
      words.map(_.probabilityCorrectByLetters.prob).product

    new Prob(probability)
  }

}
