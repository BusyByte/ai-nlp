package net.nomadicalien.nlp


import scala.annotation.tailrec
import scala.util.Random


class BruteForceNLP(stringToDecode: String, solution: String) extends NaturalLanguageProcessor with Logging {
  val encryptedSentence = new Sentence(stringToDecode)
  val solutionSentence = new Sentence(solution)
  val random = new Random()

  def process(): Sentence = {
    logger.info(s"ENCRYPTED     [$encryptedSentence]")
    logger.info(s"SOLUTION      [$solutionSentence]")
    val perms = ('a' to 'z').toList.permutations
    logSentence("INITIAL SENTENCE", encryptedSentence)
    replaceLetter(encryptedSentence, perms, encryptedSentence)
  }


  @tailrec
  private def replaceLetter(sentence: Sentence, perms: Iterator[List[Char]], maxSentence: Sentence): Sentence = {
    val sentenceLetters: List[Letter] = sentence.distinctLetters
    if (perms.isEmpty) {
      maxSentence
    } else {
      val currentPerm: List[Letter] = perms.next()
      val zippedReplacements = sentenceLetters zip currentPerm
      val newSentence = sentence.swapMultiple(zippedReplacements.toMap)

      if(random.nextInt(1000000) == 0) {
        logger.info(s"Current Perm [${currentPerm.mkString}]")
        logSentence("SANITY CHECK", newSentence)
      }

      if (newSentence == solutionSentence) {
        logger.info("!!!---Found it---!!!")
      }

      if (newSentence.probabilityCorrect > maxSentence.probabilityCorrect) {
        logSentence("NEW MAX", newSentence)
        replaceLetter(sentence, perms, newSentence)
      } else {
        replaceLetter(sentence, perms, maxSentence)
      }

    }
  }


  def logSentence(label: String, currentSentence: Sentence) = {
    logger.info(s"DECODED       [$currentSentence][${ProbFormatter.format(currentSentence.probabilityCorrect)}][$label]")
  }
}
