package net.nomadicalien.nlp

import scala.collection.immutable.SortedSet


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

  val probabilityCorrect: Prob = determineProbabilityCorrect()

  lazy val distinctLetters: List[LowerCaseLetter] = encodedString.distinct.toList.collect {
    case c: Char if c.isLetter => new LowerCaseLetter(c)
  }

  /**
   * Will swap letters without regard to case.
   */
  def swap(losingLetter: LowerCaseLetter, candidateLetter: LowerCaseLetter): Sentence = Sentence(swapString(encodedString, losingLetter, candidateLetter))

  def swapMultiple(swaps: List[(LowerCaseLetter, LowerCaseLetter)]): Sentence = {
    val losers = swaps.map {it => (it._1.lowerCaseChar, it._2.lowerCaseChar)}.toMap
    val candidates = swaps.map {it => (it._2.lowerCaseChar, it._1.lowerCaseChar)}.toMap

    Sentence(
      encodedString.map {
        theChar: Char =>
          losers.getOrElse(theChar, candidates.getOrElse(theChar, theChar))
      }
    )
  }

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
