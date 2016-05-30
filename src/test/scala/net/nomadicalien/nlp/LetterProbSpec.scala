package net.nomadicalien.nlp

class LetterProbSpec extends NLPSpec {
  "this" should {

    "verifySortByProbability" in {
      val winner = LetterProb('a', 0.6)
      val median = LetterProb('a', 0.4)
      val loser = LetterProb('a', 0.2)
      val sortedList = LetterProb.sortByProbability(List(median, loser, winner))
      List(winner, median, loser) must_== sortedList
    }

    "verifySortByLetter" in {
      val winner = LetterProb('a')
      val median = LetterProb('b')
      val loser = LetterProb('c')
      val sortedList = LetterProb.sortByLetter(List(median, loser, winner))
      List(winner, median, loser) must_== sortedList
    }
  }
}
