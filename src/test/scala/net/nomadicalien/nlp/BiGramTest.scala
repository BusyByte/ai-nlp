package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._

/**
 * User: Shawn Garner
 * Created: 5/1/13 3:16 AM
 */
class BiGramTest extends JUnitSuite {
  @Test def verifyProbOfCurrentGivenPriorTH() {
    val probOfHGivenT = BiGram.probOfAGivenB('h', 't')
    assertEquals(0.3447d, probOfHGivenT, 0.0001d)
  }

  @Test def verifyProbOfCurrentGivenPriorHT() {
    val probOfTGivenH = BiGram.probOfAGivenB('t', 'h')
    assertEquals(0.0341d, probOfTGivenH, 0.0001d)
  }
}
