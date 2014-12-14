package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.Test

/**
 * User: Shawn Garner
 * Created: 4/28/13 7:59 PM
 */
class LetterProbTest extends JUnitSuite {
  @Test def verifySortByProbability() {
    val winner = LetterProb('a', 0.6)
    val median = LetterProb('a', 0.4)
    val loser = LetterProb('a', 0.2)
    val sortedList = LetterProb.sortByProbability(List(median, loser, winner))
    assert(List(winner, median, loser) === sortedList)
  }

  @Test def verifySortByLetter() {
    val winner = LetterProb('a')
    val median = LetterProb('b')
    val loser = LetterProb('c')
    val sortedList = LetterProb.sortByLetter(List(median, loser, winner))
    assert(List(winner, median, loser) === sortedList)
  }

}
