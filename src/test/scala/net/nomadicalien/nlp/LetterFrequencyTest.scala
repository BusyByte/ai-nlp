package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._

/**
 * User: Shawn Garner
 * Created: 4/28/13 10:43 AM
 */
class LetterFrequencyTest extends JUnitSuite {

  @Test def verifyTotalProbabilityEnglishLetterFreqencies() {
    assertEquals(1.0d, LetterFrequency.englishLetterFrequencies.values.sum, 0.0001d)
  }

  @Test def verifyTotalProbabilityEnglishFirstLetterFreqencies() {
    assertEquals(1.0d, LetterFrequency.englishFirstLetterFrequencies.values.sum, 0.0001d)
  }

  @Test def verifyTotalProbabilityEnglishDoubleLetterFreqencies() {
    assertEquals(1.0d, LetterFrequency.englishDoubleLetterFrequencies.values.sum, 0.0001d)
  }

  @Test def verifyLaplaceSmoothing() {
    assertTrue(LetterFrequency.englishDoubleLetterFrequencies.get('x').get > 0.0d)
  }

  @Test def verifyGetBestMatchByLetterFrequency() {
    val experimentalFrequency = 0.13
    assert('e' === LetterFrequency.getBestMatchByLetterFrequency(experimentalFrequency, Set()).get.letter)
  }

  @Test def verifyGetBestMatchByLetterFrequencyWithExclusion() {
    val experimentalFrequency = 0.13
    assert('t' === LetterFrequency.getBestMatchByLetterFrequency(experimentalFrequency, Set('e')).get.letter)
  }

  @Test def verifyGetBestMatchByFirstLetterFrequency() {
    val experimentalFrequency = 0.17
    assert('t' === LetterFrequency.getBestMatchByFirstLetterFrequency(experimentalFrequency, Set()).get.letter)
  }

  @Test def verifyGetBestMatchByFirstLetterFrequencyWithExclusion() {
    val experimentalFrequency = 0.17
    assert('a' === LetterFrequency.getBestMatchByFirstLetterFrequency(experimentalFrequency, Set('t')).get.letter)
  }

  @Test def verifyGetBestMatchByDoubleLetterFrequency() {
    val experimentalFrequency = 0.25
    assert('s' === LetterFrequency.getBestMatchByDoubleLetterFrequency(experimentalFrequency, Set()).get.letter)
  }

  @Test def verifyGetBestMatchByDoubleLetterFrequencyWithExclusion() {
    val experimentalFrequency = 0.25
    assert('l' === LetterFrequency.getBestMatchByDoubleLetterFrequency(experimentalFrequency, Set('s')).get.letter)
  }
}
