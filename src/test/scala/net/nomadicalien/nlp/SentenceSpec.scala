package net.nomadicalien.nlp

class SentenceSpec extends NLPSpec {

  val sentence = new Sentence("The pretty red ball bounced along.")

  "this" should {

    "verifySwapMultiple" in {
      val swaps = List(('t', 'a'), ('l', 'p'))
      val swappedSentence = sentence.swapMultiple(swaps)
      "ahe lreaay red btpp bounced tpong" must_== swappedSentence.toString
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
      probability must beCloseTo(0.00487, 0.0001d)
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
