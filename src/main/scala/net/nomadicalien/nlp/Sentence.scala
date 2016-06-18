package net.nomadicalien.nlp

import cats.Show
import net.nomadicalien.nlp.Probability.Probability
import net.nomadicalien.nlp.Sentence.{ReplacementLetter, LosingLetter}
import org.apache.logging.log4j.Logger


object Sentence {
  val substituteChar : Char = 26.toChar
  val NON_ALPHA_PATTERN : String = "[^\\p{Alpha}]+"
  val ALPHA_PATTERN : String = "[\\p{Alpha}]+"
  type LosingLetter = Letter
  type ReplacementLetter = Letter

  def logSentence(label: String, currentSentence: Sentence)(implicit logger: Logger, s: Show[Sentence]) = {
    val sentenceShow = s.show(currentSentence)
    logger.info(s"[$sentenceShow][$label]")
  }

  //TODO: would have liked to have this be implicit object but can't figure out how to chain them
  implicit def sentenceShow(implicit p: Show[Probability]) = new Show[Sentence] {
    override def show(sentence: Sentence): String = {
      val probShow = p.show(sentence.probabilityCorrect)
      s"$sentence:$probShow"
    }
  }
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

case class Sentence(stringToDecode : String) extends Logging {
  lazy val encodedString: String = stringToDecode.collect {
    case c: Char if c.isLetter || c.isWhitespace => c.toLower
  }

  lazy val words: List[Word] = encodedString.split(Sentence.NON_ALPHA_PATTERN).distinct.map(new Word(_)).toList

  lazy val probabilityCorrect: Probability = determineProbabilityCorrect(words)

  lazy val distinctLetters: List[Letter] = encodedString.distinct.toList.collect {
    case c: Char if c.isLetter => c
  }

  /**
   * Will swap letters without regard to case.
   */
  def swap(losingLetter: LosingLetter, candidateLetter: ReplacementLetter): Sentence = Sentence(swapString(encodedString, losingLetter, candidateLetter))


  def swapMultiple(swaps: Map[LosingLetter, ReplacementLetter]): Sentence = {
    val reversSwaps: Map[ReplacementLetter, LosingLetter] = swaps.map {p => p.swap}
    Sentence(
      encodedString.map {
        theChar: Char =>
          swaps.getOrElse(theChar, reversSwaps.getOrElse(theChar, theChar))
      }
    )
  }

  def swapMultipleA(swaps: Array[Option[Char]]): Sentence = {
    for {
      index <- swaps.indices
      replacement <- swaps(index)
      replacementIndex :Int = replacement - 'a'
      currentChar: Char = ('a' + index).toChar
    } yield swaps.update(replacementIndex, Some(currentChar))

    Sentence(
      encodedString.map {
        theChar: Char =>
          if(theChar.isLetter) {
            val index: Int = theChar - 'a'
            swaps(index).getOrElse(theChar)
          } else {
            theChar
          }
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
    words.sortWith {(lhs, rhs)=> lhs.probabilityCorrectByWord < rhs.probabilityCorrectByWord}.head
  }

  override def toString: String  = encodedString

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: Sentence => encodedString.equals(s.encodedString)
    case _ => false
  }

  override def hashCode(): Int = encodedString.hashCode

  def printWordProbabilities(implicit w: Show[Word]) = {
    this.words.map(w.show).mkString
  }

  private def determineProbabilityCorrect(wordList: List[Word]): Probability = {
    val distinctWords = wordList.distinct
    val probability: Double =

      distinctWords.map(_.probabilityCorrectByWord).sum / distinctWords.size

    probability
  }

}
