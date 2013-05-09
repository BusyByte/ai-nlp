package net.nomadicalien.nlp

/**
 * User: Shawn Garner
 * Created: 5/8/13 7:47 PM
 */
class Prob(val prob : Double = 0.0d) {
  if(prob > 1.0001d || prob < -0.00001d) throw new IllegalStateException(s"bad calculation of a probability [$prob]")

  def format() : String =  {
    ProbFormatter.format(prob)
  }

  def >(other: Prob) : Boolean  = {
      this.prob > other.prob
  }

  def <(other: Prob) : Boolean  = {
    this.prob < other.prob
  }


}
