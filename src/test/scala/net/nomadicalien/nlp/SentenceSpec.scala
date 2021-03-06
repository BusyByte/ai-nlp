package net.nomadicalien.nlp

import net.nomadicalien.nlp.Probability.Probability

class SentenceSpec extends NLPSpec {

  val sentence = new Sentence("The pretty red ball bounced along.")

  "this" should {

    "verifyLeastLikelyWord" in {
      val testSentence = new Sentence("the first conference on the topic of artificiaq inteqqikence mas hequ at uartjoxth coqqeke in this year")
      testSentence.findLeastLikelyWord().letters must_== "inteqqikence"
    }

    "verifySwapMultiple" in {
      val swaps = Map('t' -> 'a', 'l' -> 'p')
      val swappedSentence = sentence.swapMultiple(swaps)
      swappedSentence.toString must_== "ahe lreaay red btpp bounced tpong"
    }

    "verifySwapMultipleA" in {
      val swaps = Array.fill(26)(Option.empty[Char])
      val losingIndex1: Int = 't' - 'a'
      val losingIndex2: Int = 'l' - 'a'
      swaps.update(losingIndex1, Some('a'))
      swaps.update(losingIndex2, Some('p'))

      val swappedSentence = sentence.swapMultipleA(swaps)
      swappedSentence.toString must_== "ahe lreaay red btpp bounced tpong"
    }

    "verifySwap" in {
      val swappedSentence = sentence.swap('t', 'l')
      "lhe prelly red batt bounced atong" must_== swappedSentence.toString
    }

    "verifyToString" in {
      "the pretty red ball bounced along" must_== sentence.toString
    }

    "verifyWords" in {
      val words = List("the", "pretty", "red", "ball", "bounced", "along").map(new Word(_))
      words must_== sentence.words
    }

    "verifyProbabilityCorrect" in {
      val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
      val probability: Probability = solutionSentence.probabilityCorrect
      probability must beCloseTo(1.9009e-89d, 1.0e-93d)
    }

    "verifyProbabilityCorrect on nonsense" in {
      val solutionSentence = new Sentence("ean owuae sthonunhsn th ean etsws to huewowswhd whenddwgnhsn tha andi he ihuertrea stddngn wh eawa inhu")
      val probability: Probability = solutionSentence.probabilityCorrect
      probability must_== 0.0d
    }

    "verifyEquals" in {
      val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
      val solutionSentence2 = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
      solutionSentence must_== solutionSentence2
    }

    "verifyEqualsCase" in {
      val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
      val solutionSentence2 = new Sentence("the first conference on the topic of artificial intelligence was held at dartmouth college in this year.")
      solutionSentence must_== solutionSentence2
    }

    "verifyHashesMatchCase" in {
      val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
      val solutionSentence2 = new Sentence("the first conference on the topic of artificial intelligence was held at dartmouth college in this year.")
      solutionSentence.hashCode() must_== solutionSentence2.hashCode()
    }

    "verifyHashCode" in {
      val solutionSentence = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
      val solutionSentence2 = new Sentence("The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year.")
      solutionSentence.hashCode() must_== solutionSentence2.hashCode()
    }

  }

}
