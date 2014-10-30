package net.nomadicalien.nlp


/**
 * User: Shawn Garner
 * Created: 4/16/13 10:22 PM
 */
class NLP2(stringToDecode: String, solution: String) extends Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)

  def process() = {
    logger.info(s"ENCRYPTED     [$encryptedSentence]")
    logger.info(s"SOLUTION      [$solutionSentence]")
    val allCandidateReplacements = ('a' to 'z').toList.map(LowerCaseLetter)
    logCurrent(encryptedSentence)
    replaceLetter(encryptedSentence, allCandidateReplacements, 0, encryptedSentence)
  }


   def replaceLetter(sentence: Sentence, candidatesChars: List[LowerCaseLetter], distinctLettersIndex: Int, maxSentence: Sentence): Sentence = {
     val distinctLetters = sentence.distinctLetters
     if(distinctLettersIndex >= distinctLetters.size) {
       if (sentence == solutionSentence) {
         logger.info("!!!---Found it---!!!")
         throw new RuntimeException("!!!---Found it---!!!")
       }
       if (sentence.probabilityCorrect > maxSentence.probabilityCorrect) {
         logCurrent(sentence)
         sentence
       } else {
         maxSentence
       }
     } else {
        val letterToReplace = distinctLetters(distinctLettersIndex)
        candidatesChars.foldLeft(maxSentence)(
          (currMaxSentence, replacementLetter) => replaceLetter(sentence.swap(letterToReplace, replacementLetter), candidatesChars.filter(_ != replacementLetter), distinctLettersIndex + 1, currMaxSentence)
        )
    }
  }


  def logCurrent(currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${currentSentence.probabilityCorrect.format()}]")
  }
}
