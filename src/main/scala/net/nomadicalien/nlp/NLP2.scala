package net.nomadicalien.nlp

import scala.collection.mutable
import org.apache.commons.lang3.mutable.{MutableInt, MutableDouble}
import scala.Double
import java.util.regex.Pattern
import scala.util.control.Breaks._
import net.nomadicalien.nlp.WordFrequency.WordRanking
import scala.collection.mutable.ListBuffer

/**
 * User: Shawn Garner
 * Created: 4/16/13 10:22 PM
 */
class NLP2(stringToDecode : String) extends Randomness with Logging {

  def process {
    val visitedLetters = mutable.Map[Int, mutable.Set[Char]]()
    var sentence = new Sentence(stringToDecode)

    while(!sentence.matches()) {
      val sentenceProb : Double = calculateProbablilitySentenceIsCorrect(sentence)
      logger.info("sentence prob correct = " + ProbFormatter.format(sentenceProb))
      if (sentenceProb > 0.6d) {
        throw new CloseEnoughMatchException("Probability Correct is " + ProbFormatter.format(sentenceProb))
      }

      val probabilities : Map[Char, Double] = calculateCharacterProbabilities(sentence)
      printWordProbabilities(sentence, probabilities)

      val lastSentenceDecoded : String  = sentence.toString()

      val leastLikelyCorrect : Option[Char] = findLeastLikelyCorrectLetter(probabilities, Set())
      if (leastLikelyCorrect.isEmpty) {
        throw new OutOfOptionsException("Could not fine least likely correct")
      }
      val indexOfLeastLikely : Int = lastSentenceDecoded.indexOf(leastLikelyCorrect.get.toString())
      val excludedLetters : mutable.Set[Char] = visitedLetters.getOrElseUpdate(indexOfLeastLikely, mutable.Set[Char]())
      excludedLetters.add(leastLikelyCorrect.get)

      val replacement : Option[Char] = findLeastLikelyCorrectLetter(probabilities, excludedLetters.toSet)
      if(replacement.isEmpty) {
        throw new OutOfOptionsException("could not find a replacement character")
      }
      sentence = new Sentence(lastSentenceDecoded.replace(leastLikelyCorrect.get, replacement.get))
      sentence.logTranslation(lastSentenceDecoded)
    }


  }

  def calculateCharacterProbabilities(sentence: Sentence) : Map[Char, Double] = {
    val probs = mutable.Map[Char, MutableDouble]()
    val letterCount = mutable.Map[Char, MutableInt]()

    sentence.words.foreach { word : String =>
      var priorChar : Char = '0'
      var priorProbability : Double = 0.0d
      word.zipWithIndex.foreach { case(theChar, index) =>
        val probability : MutableDouble = probs.getOrElse(theChar, new MutableDouble(0.0d))
        val theLettersCount : MutableInt = letterCount.getOrElse(theChar, new MutableInt(0))

        if (index == 0) {
          probability.add(LetterFrequency.firstLetterProbabilityOf(theChar).getOrElse(0.0d))
        } else if(sentence.isDoubleLetter(theChar)){
          probability.add(LetterFrequency.doubleLetterProbabilityOf(theChar).getOrElse(0.0d))
        } else {
          probability.add(priorProbability * BiGram.getPreciseBigramByChars(priorChar, theChar))
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

  def determineMaxProbReplacement(leastLikelyCorrect : CharProb, probabilities : Map[Char, Double], excludedLetters : Set[Char]) : Option[CharProb] = {
    val totalExcluded = excludedLetters + leastLikelyCorrect.letter

    val replacementCandidates = ('a' to 'z').filterNot(totalExcluded.contains(_)).map {theChar: Char => CharProb(theChar,probabilities.getOrElse(theChar, 0.0d))}.toList

    val sortedReplacementCandidates = CharProb.sortByProbability(replacementCandidates)

    sortedReplacementCandidates.headOption

  }

  //TODO: need a unit test
  def findLeastLikelyCorrectLetter(probabilities : Map[Char, Double], excludedLetters : Set[Char]) : Option[Char] = {
    val reducedProbabilities : Map[Char, Double] =  probabilities.filterNot {case(theChar:Char, _) => excludedLetters.contains(theChar)}
    val sortedEntries : List[Char] = reducedProbabilities.toList.sortBy(_._2).map(_._1).toList
    sortedEntries.headOption
  }

  def letterProbabilityCorrect(foundWord : String, probabilities : Map[Char, Double]) : Double = {
    val probabilityCorrect : Double = foundWord.map {probabilities.getOrElse(_, 0.0d)}.sum / foundWord.length


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

    val wordRankingList : List[WordRanking] = WordFrequency.sizeToWordRankingMap.getOrElse(wordSize, ListBuffer()).toList

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
}
