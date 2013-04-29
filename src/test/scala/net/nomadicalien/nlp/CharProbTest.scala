package net.nomadicalien.nlp

import org.scalatest.junit.JUnitSuite
import org.junit.Test

/**
 * User: Shawn Garner
 * Created: 4/28/13 7:59 PM
 */
class CharProbTest extends JUnitSuite {
  @Test def verifySortByProbability() {
    val winner = CharProb('a', 0.6)
    val median = CharProb('a', 0.4)
    val loser = CharProb('a', 0.2)
    val sortedList = CharProb.sortByProbability(List(median, loser, winner))
    assert(List(winner, median, loser) === sortedList)
  }

  @Test def verifySortByLetter() {
    val winner = CharProb('a')
    val median = CharProb('b')
    val loser = CharProb('c')
    val sortedList = CharProb.sortByLetter(List(median, loser, winner))
    assert(List(winner, median, loser) === sortedList)
  }

}
