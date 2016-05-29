package net.nomadicalien.nlp

import java.util.regex.Pattern

import net.nomadicalien.nlp.WordFrequency.WordRanking

object Word {
  val vowelPattern = Pattern.compile("(.*)([aAeEiIoOuUyY]+)(.*)")
  val onlyVowelPattern = Pattern.compile("[aAeEiIoOuUyY]+")
  val onlyConsonantsPattern = Pattern.compile("[bBcCdDfFgGhHjJkKlLmMnNpPqQrRsStTvVxXzZwWyY]+")
}

case class Word(letters: String) {
  lazy val hasVowel = Word.vowelPattern.matcher(letters).matches()

  lazy val hasAllVowels =  Word.onlyVowelPattern.matcher(letters).matches()

  lazy val hasAllConsonants = Word.onlyConsonantsPattern.matcher(letters).matches()

  lazy val probabilityCorrectByLetters: Probability = determineProbCorrect(determineCharProbs())

  lazy val probabilityCorrectByWord: Probability = determineProbabilityWordIsCorrect()

  private def determineCharProbs(): List[LetterProb] = {
    val (_, reverseCharProbList) = letters.foldLeft((None: Option[Char], List[LetterProb]()))(
      (accumulator, currentLetter) => {
        val priorLetterMaybe: Option[Char] = accumulator._1
        val currentList = accumulator._2
        val prob: Double = {
          if (priorLetterMaybe.isEmpty) {
            LetterFrequency.firstLetterProbabilityOf(currentLetter).getOrElse(0.0d)
          } else {
            val priorLetter = priorLetterMaybe.get
            if (currentLetter == priorLetter) {
              LetterFrequency.doubleLetterProbabilityOf(currentLetter).getOrElse(0.0d)
            } else {
              BiGram.biGramLookup(currentLetter + "|" + priorLetter)
            }
          }
        }

        (Some(currentLetter), LetterProb(currentLetter, prob) :: currentList)
      }
    )

    reverseCharProbList // probabilities are reverse order but should not matter since we only multiply them
  }

  private def determineProbCorrect(charProbs: List[LetterProb]):Probability = charProbs.map(_.probability).product

  private def determineProbabilityWordIsCorrect(): Probability = {

    val wordSize = letters.length

    if (hasAllConsonants || (wordSize > 1 && hasAllVowels) || !hasVowel) {
      return 0.0d
    }
    val wordRankingList: List[WordRanking] = WordFrequency.getRankingList(wordSize)

    val wordRanking: Option[WordRanking] = wordRankingList.find {
      theWordRanking: WordRanking =>
        theWordRanking.word.equalsIgnoreCase(letters)//TODO: move to find method in WordFrequency to search by word
    }

    val probabilityCorrect : Double = {
      if (wordRanking.isDefined) {
        wordRanking.get.probability
      } else {
        val foundWord = KnownWords.findWord(letters)
        if (foundWord) {
          1.0d / KnownWords.numberWordsOfSize(wordSize)
        } else {
          math.max(probabilityCorrectByLetters, 1.0d / WordFrequency.ESTIMATE_NUMBER_WORDS_IN_ENGLISH)
        }
      }
    }

    probabilityCorrect
  }

  def format(): String = {
    val sb = new StringBuilder()
    sb.append("\n")
    sb.append(this.toString())
    sb.append("\n")
    sb.append("letter prob")
    sb.append("=")
    sb.append(ProbFormatter.format(probabilityCorrectByLetters))
    sb.append(",")
    sb.append("word prob")
    sb.append("=")
    sb.append(ProbFormatter.format(probabilityCorrectByWord))
    sb.append("\n\n")

    sb.toString()
  }

  override def toString: String = letters

}
