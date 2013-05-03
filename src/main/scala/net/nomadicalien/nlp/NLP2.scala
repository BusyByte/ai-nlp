package net.nomadicalien.nlp

import scala.collection.mutable
import org.apache.commons.lang3.mutable.{MutableInt, MutableDouble}
import scala.Double
import java.util.regex.Pattern
import net.nomadicalien.nlp.WordFrequency.WordRanking

/**
 * User: Shawn Garner
 * Created: 4/16/13 10:22 PM
 */
class NLP2(stringToDecode : String, solution: String) extends Randomness with Logging {

  def process {
    var sentence = new Sentence(stringToDecode)

    while(sentence.toString() != solution) {

      val sentenceProb : Double = calculateProbablilitySentenceIsCorrect(sentence)
      logger.info("sentence prob correct = " + ProbFormatter.format(sentenceProb))
      if (sentenceProb > 0.6d) { throw new CloseEnoughMatchException("Probability Correct is " + ProbFormatter.format(sentenceProb)) }

      val probabilities : Map[Char, Double] = calculateCharacterProbabilities(sentence)
      printWordProbabilities(sentence, probabilities)

      val lastSentenceDecoded : String  = sentence.toString()

      val leastLikelyCorrect : Char = findLeastLikelyCorrectLetter(probabilities)

      val replacement : Char = determineMaxProbReplacement(leastLikelyCorrect, probabilities)

      sentence = sentence.swap(leastLikelyCorrect, replacement)
      logTranslation(lastSentenceDecoded, sentence.toString())
    }
  }

  def calculateCharacterProbabilities(sentence: Sentence) : Map[Char, Double] = {
    val probs = mutable.Map[Char, MutableDouble]()
    val letterCount = mutable.Map[Char, MutableInt]()

    sentence.words.foreach { word : String =>
      var priorChar : Char = '0'
      var priorProbability : Double = 0.0d
      word.zipWithIndex.foreach { case(theChar, index) =>
        val probability : MutableDouble = probs.getOrElseUpdate(theChar, new MutableDouble(0.0d))
        val theLettersCount : MutableInt = letterCount.getOrElseUpdate(theChar, new MutableInt(0))

        if (index == 0) {
          probability.add(LetterFrequency.firstLetterProbabilityOf(theChar).getOrElse(0.0d))
        } else if(sentence.isDoubleLetter(theChar)){
          probability.add(LetterFrequency.doubleLetterProbabilityOf(theChar).getOrElse(0.0d))
        } else {
          probability.add(priorProbability * BiGram.probOfCurrentGivenPrior(priorChar, theChar))
        }

        theLettersCount.increment()
        priorProbability =  probability.doubleValue() / theLettersCount.doubleValue()
        priorChar = theChar
      }
    }

    probs.foreach {f: (Char, MutableDouble)  =>
      val probability : MutableDouble = f._2
      val theLetterCount : MutableInt = letterCount.get(f._1).getOrElse(new MutableInt(0))
      val newProbability = probability.doubleValue() / theLetterCount.doubleValue()
      probability.setValue(newProbability)
    }


    probs.toList.map { f:(Char, MutableDouble) => (f._1, f._2.doubleValue())}.toMap
  }


  def calculateProbablilitySentenceIsCorrect(sentence : Sentence) : Double = {
    val probabilities : Map[Char, Double] = calculateCharacterProbabilities(sentence)
    val probability : Double =
      sentence.words.map {  word : String =>
        var wordProb : Double = 1.0d
        word.foreach {theChar : Char =>
          wordProb *= probabilities.get(theChar).getOrElse(0.0d)
        }
        wordProb
      }.sum / sentence.words.length

    probability
  }


  def determineMaxProbReplacement(leastLikelyCorrect : Char, probabilities : Map[Char, Double]) : Char = {
    val allReplacementCandidates = mutable.Set[Char]()

    val sortedByProb = probabilities.toList.sortBy {case(theChar: Char, prob : Double) => 1.0d - prob}
    val numLetters = sortedByProb.size

    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.25d * numLetters).toInt))._1)

    allReplacementCandidates.add(sortedByProb(nextInt((0.35d * numLetters).toInt))._1)
    allReplacementCandidates.add(sortedByProb(nextInt((0.35d * numLetters).toInt))._1)

    allReplacementCandidates.add(sortedByProb(nextInt(numLetters))._1)

    val lettersNotInPhrase = ('a' to 'z').toSet.--(probabilities.keySet)
    allReplacementCandidates.++=(lettersNotInPhrase)
    allReplacementCandidates.-=(leastLikelyCorrect)
    val allReplacementCandidateList = allReplacementCandidates.toList
    allReplacementCandidateList(nextInt(allReplacementCandidateList.size))
  }

  //TODO: need a unit test
  def findLeastLikelyCorrectLetter(probabilities : Map[Char, Double]) : Char = {
    val sortedEntries : List[Char] = probabilities.toList.sortBy(_._2).map(_._1).toList
    sortedEntries.head
  }

  def letterProbabilityCorrect(foundWord : String, probabilities : Map[Char, Double]) : Double = {
    val probabilityCorrect : Double = foundWord.map {probabilities.getOrElse(_, 0.0d)}.product
    probabilityCorrect
  }

  val vowelPattern = Pattern.compile("(.*)([aAeEiIoOuUyY]+)(.*)")
  val onlyVowelPattern = Pattern.compile("[aAeEiIoOuUyY]+")

  def hasVowel(wholeDecriptedWordString : String) : Boolean = {vowelPattern.matcher(wholeDecriptedWordString).matches()}
  def hasAllVowels(wholeDecriptedWordString : String) : Boolean = {onlyVowelPattern.matcher(wholeDecriptedWordString).matches()}

  def determineProbabilityWordIsCorrect(wholeDecryptedWordString : String,  probabilities : Map[Char, Double]) : Double =  {

    var probabilityCorrect = 0.0d
    val wordSize = wholeDecryptedWordString.length

    if (!hasVowel(wholeDecryptedWordString) || (wordSize > 1 && hasAllVowels(wholeDecryptedWordString))) {
      return probabilityCorrect
    }

    val wordRankingList : List[WordRanking] = WordFrequency.getRankingList(wordSize)

    val wordRanking : Option[WordRanking] =  wordRankingList.find { theWordRanking : WordRanking =>
      theWordRanking.word.equalsIgnoreCase(wholeDecryptedWordString)
    }

    if(wordRanking.isDefined) {
      probabilityCorrect = wordRanking.get.probability
    } else {
      val foundWord = KnownWords.findWord(wholeDecryptedWordString)
      if (foundWord) {
        val numWordsOfSize = KnownWords.numberWordsOfSize(wordSize)
        probabilityCorrect = (1.0d / numWordsOfSize)
      } else {
        probabilityCorrect = (1.0d / WordFrequency.ESTIMATE_NUMBER_WORDS_IN_ENGLISH)
        probabilityCorrect = Math.max(probabilityCorrect, letterProbabilityCorrect(wholeDecryptedWordString, probabilities))
      }

    }

    probabilityCorrect
  }

  def printWordProbabilities(sentence : Sentence , probabilities : Map[Char, Double]) {
    val sb = new StringBuilder()
    sentence.words.foreach { foundWord : String =>
      sb.append("\n")
      sb.append(foundWord)
      sb.append("\n")
      sb.append("letter prob")
      sb.append("=")
      sb.append(ProbFormatter.format(letterProbabilityCorrect(foundWord, probabilities)))
      sb.append(",")
      sb.append("word prob")
      sb.append("=")
      sb.append(ProbFormatter.format(determineProbabilityWordIsCorrect(foundWord, probabilities)))
      sb.append("\n")
      foundWord.foreach { letter : Char =>
        sb.append("[")
        sb.append(letter)
        sb.append("=")
        sb.append(ProbFormatter.format(probabilities.getOrElse(letter, 0.0d)))
        sb.append("]")
      }
      sb.append("\n\n")
    }

    logger.debug(sb.toString())
  }


  def logTranslation(lastSentenceDecoded : String, currentSentenceDecoded : String) {
    logger.info(s"ENCRYPTED     [$stringToDecode]")
    if (lastSentenceDecoded != null) {
      logger.info(s"LAST DECODED  [$lastSentenceDecoded]")
    }
    logger.info(s"DECODED       [$currentSentenceDecoded]")
    logger.info(s"SOLUTION      [$solution]")
  }
}
