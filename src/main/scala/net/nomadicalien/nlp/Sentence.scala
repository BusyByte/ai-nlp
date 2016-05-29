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
case class Sentence(stringToDecode : String) extends Logging {
  val encodedString: String = stringToDecode.collect {
    case c: Char if c.isLetter || c.isWhitespace => c.toLower
  }

  val words: List[Word] = encodedString.split(Sentence.NON_ALPHA_PATTERN).distinct.map(new Word(_)).toList

  val probabilityCorrect: Probability = determineProbabilityCorrect(words)

  lazy val distinctLetters: List[Letter] = encodedString.distinct.toList.collect {
    case c: Char if c.isLetter => c
  }

  /**
   * Will swap letters without regard to case.
   */
  def swap(losingLetter: Letter, candidateLetter: Letter): Sentence = Sentence(swapString(encodedString, losingLetter, candidateLetter))

  def swapMultiple(swaps: List[(Letter, Letter)]): Sentence = {
    val losers = swaps.map {it => (it._1, it._2)}.toMap
    val candidates = swaps.map {it => (it._2, it._1)}.toMap

    Sentence(
      encodedString.map {
        theChar: Char =>
          losers.getOrElse(theChar, candidates.getOrElse(theChar, theChar))
      }
    )
  }

  private def swapString(input: String, losingLetter: Letter, candidateLetter: Letter): String = {
    if(losingLetter == candidateLetter)
      input
    else
      input.map {
        case c: Letter if c == losingLetter => candidateLetter
        case c: Letter if c == candidateLetter => losingLetter
        case c: Letter => c
      }
  }

  def findLeastLikelyWord() : Word = {
    words.sortWith {(lhs, rhs)=> lhs.probabilityCorrectByLetters < rhs.probabilityCorrectByLetters}.head
  }

  override def toString: String  = encodedString



  override def equals(obj: scala.Any): Boolean = obj match {
    case s: Sentence => encodedString.equals(s.encodedString)
    case _ => false
  }

  override def hashCode(): Int = encodedString.hashCode

  def printWordProbabilities() = {
    this.words.map(_.format()).mkString
  }

  private def determineProbabilityCorrect(wordList: List[Word]): Probability = {
    val distinctWords = wordList.distinct
    val probability: Double =

      distinctWords.map(_.probabilityCorrectByWord).sum / distinctWords.size

    probability
  }

}
