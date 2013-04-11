package net.nomadicalien.nlp

/**
 * @author Shawn Garner
 * Created: 3/30/13 6:43 AM
 */
object LetterFequency {
  val englishLetterFrequencies = Map(
    'a' -> 0.08167d,
    'b' -> 0.01492d,
    'c' -> 0.02782d,
    'd' -> 0.04253d,
    'e' -> 0.12702d,
    'f' -> 0.02228d,
    'g' -> 0.02015d,
    'h' -> 0.06094d,
    'i' -> 0.06966d,
    'j' -> 0.00153d,
    'k' -> 0.00772d,
    'l' -> 0.04025d,
    'm' -> 0.02406d,
    'n' -> 0.06749d,
    'o' -> 0.07507d,
    'p' -> 0.01929d,
    'q' -> 0.00095d,
    'r' -> 0.05987d,
    's' -> 0.06327d,
    't' -> 0.09056d,
    'u' -> 0.02758d,
    'v' -> 0.00978d,
    'w' -> 0.02360d,
    'x' -> 0.00150d,
    'y' -> 0.01974d,
    'z' -> 0.00074d
  )

  val englishFirstLetterFrequencies = Map(
    'a' -> 0.11602d,
    'b' -> 0.04702d,
    'c' -> 0.03511d,
    'd' -> 0.02670d,
    'e' -> 0.02000d,
    'f' -> 0.03779d,
    'g' -> 0.01950d,
    'h' -> 0.07232d,
    'i' -> 0.06286d,
    'j' -> 0.00631d,
    'k' -> 0.00690d,
    'l' -> 0.02705d,
    'm' -> 0.04374d,
    'n' -> 0.02365d,
    'o' -> 0.06264d,
    'p' -> 0.02545d,
    'q' -> 0.00173d,
    'r' -> 0.01653d,
    's' -> 0.07755d,
    't' -> 0.16671d,
    'u' -> 0.01487d,
    'v' -> 0.00619d,
    'w' -> 0.06661d,
    'x' -> 0.00005d,
    'y' -> 0.01620d,
    'z' -> 0.00050d
  )

  val englishDoubleLetterFrequencies = createDoubleLetterFrequencyMap()

  /**
   * most common double letters LL EE SS OO TT FF RR NN PP CC, <br>
   * MM, GG also seem high, DD, CC, BB, ZZ seem relatively high, <br>
   * UU, AA, HH, II, KK, VV, WW are relatively low, <br>
   * JJ, YY, QQ, and XX are relatively non-existent but want to smooth because
   * probably not zero
   */
  def createDoubleLetterFrequencyMap() : Map[Char, Double] = {
    val freqMap = scala.collection.mutable.Map(
      's' -> 10600,
        'l' -> 8300,
        'e' -> 3900,
        'o' -> 3800,
        't' -> 3100,
        'r' -> 2500,
        'n' -> 1700,
        'p' -> 1900,
        'm' -> 1800,
        'f' -> 1700,
        'g' -> 1300,

        'd' -> 970,
        'c' -> 960,
        'b' -> 950,
        'z' -> 350,

        'u' -> 30,
        'a' -> 75,
        'h' -> 45,
        'i' -> 55,
        'k' -> 50,
        'v' -> 25,
        'w' -> 25,

        'j' -> 5,
        'y' -> 5,
        'q' -> 0,
        'x' -> 0
    )

    var totalObservationCount : Int = 0
    freqMap.values.foreach {count: Int => totalObservationCount = totalObservationCount + count}
    freqMap.mapValues {count:Int => laplaceSmooth(count,totalObservationCount)}.toMap
  }

  /**
   * P(x)= (count(x)+k)/(N + k|x|)
   */
   def laplaceSmooth(countOfX : Double, totalObservationCount : Double) : Double = {
      val smoothingParameterK = 2.0d//estimated 2 unknown observations
      val numerator = countOfX + smoothingParameterK
      val numberValuesDoubleLetterCanTakeOn = 26.0d
      val denominator = totalObservationCount + (smoothingParameterK * numberValuesDoubleLetterCanTakeOn)
      (numerator / denominator)
   }


  def probabilityOf(target : Char) : Option[Double] = {
    englishLetterFrequencies.get(target)
  }

  def firstLetterProbabilityOf(target : Char) : Option[Double] = {
    englishFirstLetterFrequencies.get(target)
  }

  def doubleLetterProbabilityOf(target : Char) : Option[Double] = {
    englishDoubleLetterFrequencies.get(target)
  }

  def getBestMatchByFirstLetterFrequency(experimentalFrequency : Double, exclusions : Set[Char]) : CharProb = {
    getBestMatchByFrequency(englishFirstLetterFrequencies, experimentalFrequency, exclusions)
  }

  def getBestMatchByLetterFrequency(experimentalFrequency : Double, exclusions : Set[Char]) : CharProb =  {
    getBestMatchByFrequency(englishLetterFrequencies, experimentalFrequency, exclusions)
  }

  def getBestMatchByDoubleLetterFrequency(experimentalFrequency : Double, exclusions : Set[Char]) : CharProb = {
    getBestMatchByFrequency(englishDoubleLetterFrequencies, experimentalFrequency, exclusions);
  }

  private def getBestMatchByFrequency(mapToSearch : Map[Char, Double], experimentalFrequency : Double,
  excludedLetters : Set[Char]) : CharProb = {

    val lowestFound : (Char, Double) =  mapToSearch.filterKeys {key : Char => excludedLetters.contains(key)}.minBy {p: (Char, Double) =>
      val theoreticalFrequency = p._2
      val numerator : Double =  Math.abs(experimentalFrequency - theoreticalFrequency)
      val denominator : Double = Math.max(Math.abs(experimentalFrequency), Math.abs(theoreticalFrequency))
      (numerator/denominator)
    }
    CharProb(lowestFound._1, lowestFound._2)
  }

}
