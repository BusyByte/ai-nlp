package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._

/**
 * User: Shawn Garner
 * Created: 4/22/13 8:46 PM
 */
class WordFrequencyTest extends JUnitSuite {

  @Before def setup() {

  }

  @Test def verifyEasy() { // Uses JUnit-style assertions
      val rank = WordFrequency.getRankingList(3).head
      assertEquals("the", rank.word)
      assertEquals(1, rank.rank)
      assertTrue(WordFrequency.ONE_THIRD == rank.probability)
  }

  @Test def verifyEmpty() { // Uses ScalaTest assertions
    assertTrue(WordFrequency.getRankingList(200).isEmpty)
  }
}
