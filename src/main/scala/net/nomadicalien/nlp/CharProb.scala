package net.nomadicalien.nlp

/**
 * @author Shawn Garner
 * Created: 3/30/13 7:16 AM
 */
case class CharProb(letter : Char, probability : Double = 0.0d)

object CharProb {
  /** highest probability should be first **/
  def sortByProbability(listToSort : List[CharProb]) : List[CharProb] = {
      listToSort.sortBy(_.probability)
  }

  /** a should be first and z should be last **/
  def sortByLetter(listToSort : List[CharProb]) : List[CharProb] = {
    listToSort.sortBy(_.letter)
  }

}

