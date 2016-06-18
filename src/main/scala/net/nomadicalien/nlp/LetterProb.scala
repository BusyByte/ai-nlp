package net.nomadicalien.nlp

import net.nomadicalien.nlp.Probability.Probability

case class LetterProb(letter: Letter, probability: Probability = 0.0d)

object LetterProb {
  /** highest probability should be first **/
  def sortByProbability(listToSort : List[LetterProb]) : List[LetterProb] = {
      listToSort.sortBy(1.0d - _.probability)
  }

  /** a should be first and z should be last **/
  def sortByLetter(listToSort : List[LetterProb]) : List[LetterProb] = {
    listToSort.sortBy(_.letter)
  }

}

