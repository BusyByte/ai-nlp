package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._

/**
 * User: Shawn Garner
 * Created: 4/25/13 10:10 PM
 */
class SentenceTest extends JUnitSuite {

  val sentence = new Sentence("The pretty red ball bounced along.")

  @Test def verifySwap() {
    val swappedSentence = sentence.swap('t', 'l')
    assertEquals("Lhe prelly red batt bounced atong.", swappedSentence.toString())
  }

  @Test def verifyToString() {
    assertEquals("The pretty red ball bounced along.", sentence.toString())
  }

  @Test def verifyWords() {
    val words = List("the", "pretty", "red", "ball", "bounced", "along")
    assertEquals(words, sentence.words)
  }

  @Test def verifyLetterFrequencyMapLetters() {
    val letters = Set('t', 'h', 'e', 'p', 'r', 'y', 'd', 'b', 'a', 'o', 'u', 'n', 'c', 'l', 'g')
    assertEquals(letters, sentence.frequencyMap.keySet)
  }

  @Test def verifyLetterFrequencyMapTotalProbability() {
     assertEquals(1.0d, sentence.frequencyMap.values.sum, 0.0001d);
  }

  @Test def verifyFirstLetterFrequencyMapLetters() {
    val letters = Set('t', 'p', 'r', 'b', 'a')
    assertEquals(letters, sentence.firstLetterFrequencyMap.keySet)
  }

  @Test def verifyFistLetterFrequencyMapTotalProbability() {
    assertEquals(1.0d, sentence.firstLetterFrequencyMap.values.sum, 0.0001d);
  }

  @Test def verifyDoubleLetterFrequencyMapLetters() {
    val letters = Set('t', 'l')
    assertEquals(letters, sentence.doubleLetters)
  }

  @Test def verifyIsDoubleLetter() {
    assertTrue(sentence.isDoubleLetter('t'))
  }

  @Test def verifyIsNotDoubleLetter() {
    assertFalse(sentence.isDoubleLetter('a'))
  }


}
