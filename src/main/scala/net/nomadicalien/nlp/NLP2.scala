package net.nomadicalien.nlp


/**
 * User: Shawn Garner
 * Created: 4/16/13 10:22 PM
 */
class NLP2(stringToDecode: String, solution: String) extends Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)

  def process() = {


//      logTranslation(lastSentence, currentSentence)
//      val sentenceProb: Prob = currentSentence.probabilityCorrect
//      logger.info("sentence prob correct = " + sentenceProb.prob)

  //    currentSentence.printWordProbabilities()

//      val leastLikelyWord = currentSentence.findLeastLikelyWord.toString()
  //    logger.debug(s"least likely word [$leastLikelyWord]")
      val allCandidateReplacements = ('a' to 'z').toList.map(LowerCaseLetter(_))
      replaceLetter(encryptedSentence, allCandidateReplacements, 0, List())
  }

  def replaceLetter(sentence : Sentence, candidatesChars: List[LowerCaseLetter], index: Int, accumulatedSentences: List[Sentence]) : List[Sentence] = {
    val distinctLetters = encryptedSentence.distinctLetters
    index >= distinctLetters.size match {
      case true => accumulatedSentences
      _ =>  ???
    }

  }


  def logTranslation(lastSentence: Sentence, currentSentence: Sentence) = {
    logger.info(s"ENCRYPTED     [$encryptedSentence]")
    logger.info(s"LAST DECODED  [$lastSentence]")
    logger.info(s"DECODED       [$currentSentence]")
    logger.info(s"SOLUTION      [$solutionSentence]")
  }
}
