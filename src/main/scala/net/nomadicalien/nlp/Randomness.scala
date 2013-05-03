package net.nomadicalien.nlp

/**
 * User: Shawn Garner
 * Created: 4/20/13 8:26 AM
 */
trait Randomness {
  val random = new java.util.Random()
  def nextInt(exclusiveUpperLimit : Int) = Math.abs(random.nextInt(exclusiveUpperLimit))
}
