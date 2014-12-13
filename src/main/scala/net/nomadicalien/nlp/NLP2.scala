package net.nomadicalien.nlp


import scala.annotation.tailrec


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
    val perms = ('a' to 'z').toList.permutations
    logSentence("INITIAL SENTENCE", encryptedSentence)
    replaceLetter(encryptedSentence, perms, encryptedSentence, 0)
  }


  @tailrec
  private def replaceLetter(sentence: Sentence, perms: Iterator[List[Char]], maxSentence: Sentence, stepCount: Long): Sentence = {
    val sentenceLetters: List[LowerCaseLetter] = sentence.distinctLetters
    if (perms.isEmpty) {
      maxSentence
    } else {
      val currentPerm = perms.next().map(new LowerCaseLetter(_))
      val zippedReplacements = sentenceLetters zip currentPerm
      val newSentence = sentence.swapMultiple(zippedReplacements)

      if(stepCount % 1000000 == 0) {
        logger.info(s"Current Perm [${currentPerm.mkString}]")
        logSentence("SANITY CHECK", newSentence)
      }

      if (newSentence == solutionSentence) {
        logger.info("!!!---Found it---!!!")
      }

      if (newSentence.probabilityCorrect > maxSentence.probabilityCorrect) {
        logSentence("NEW MAX", newSentence)
        replaceLetter(sentence, perms, newSentence, stepCount + 1)
      } else {
        replaceLetter(sentence, perms, maxSentence, stepCount + 1)
      }

    }
  }


  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${currentSentence.probabilityCorrect.format()}][$label]")
  }
}
