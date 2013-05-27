package net.nomadicalien.nlp


object Sentence {
  val substituteChar : Char = 26.toChar
  val NON_ALPHA_PATTERN : String = "[^\\p{Alpha}]+"
  val ALPHA_PATTERN : String = "[\\p{Alpha}]+"

}

/**
 * User: Shawn Garner
 * Created: 4/11/13 10:49 PM
 */
class Sentence(stringToDecode : String) extends Logging {
  val theHashCode = stringToDecode.hashCode()

  val words : Set[Word] = stringToDecode.toLowerCase.split(Sentence.NON_ALPHA_PATTERN).map(new Word(_)).toSet

  val probabilityCorrect : Prob = determineProbabilityCorrect()

  /**
   * Will swap letters without regard to case. <br>
   * Will maintain the case of the letter positions of the original sentence.
   */
  def swap(losingLetter : Char, candidateLetter : Char) : Sentence = {
    logger.debug(s"replacing ${losingLetter} with ${candidateLetter}")

    val upperCaseCharIndexes : Set[Int] = stringToDecode.zipWithIndex.filter(_._1.isUpper).map(_._2).toSet

    val newSentenceLowerCase = stringToDecode.toLowerCase.replace(candidateLetter.toLower, Sentence.substituteChar).replace(losingLetter.toLower, candidateLetter.toLower).replace(Sentence.substituteChar, losingLetter.toLower)
    val newSentence = newSentenceLowerCase.zipWithIndex.map { it => if(upperCaseCharIndexes.contains(it._2)) {it._1.toUpper} else {it._1} }.mkString

    new Sentence(newSentence)
  }

  def findLeastLikelyWord() : Word = {
    val wordList: List[Word] = words.toList
    wordList.sortWith {(lhs, rhs)=> lhs.probabilityCorrectByLetters < rhs.probabilityCorrectByLetters}.head
  }

  override def toString() : String  = {
    stringToDecode
  }
  override def equals(obj : Any) : Boolean = {
    if(obj != null) { stringToDecode == obj.toString }
    else {false}
  }

  override def hashCode() : Int = {
    theHashCode
  }

  def printWordProbabilities() {
    val sb = new StringBuilder()
    this.words.foreach {
      word => sb.append(word.format())
    }

    logger.debug(sb.toString())
  }

  private def determineProbabilityCorrect(): Prob = {
    val probability: Double =
      words.map(_.probabilityCorrectByLetters.prob).product

    new Prob(probability)
  }

}
