package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.{Ignore, Test}
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
    val words = Set("the", "pretty", "red", "ball", "bounced", "along").map(new Word(_))
    assert(words === sentence.words)
  }

  @Ignore @Test def verifyProbabilityCorrect() {
    val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
    val probability: Prob = solutionSentence.probabilityCorrect
    assertEquals(0.0049, probability.prob, 0.0001d)
  }

  @Test def verifyEquals() {
    val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
    val solutionSentence2 = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
    assert(solutionSentence === solutionSentence2)
  }

  @Test def verifyHashCode() {
    val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
    val solutionSentence2 = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
    assert(solutionSentence.hashCode() === solutionSentence2.hashCode())
  }



}
