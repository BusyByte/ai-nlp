package net.nomadicalien.nlp


class LetterFrequencySpec extends NLPSpec {
  "this" should {
    "verifyTotalProbabilityEnglishLetterFrequencies" in {
      LetterFrequency.englishLetterFrequencies.values.sum must beCloseTo(1.0d, 0.0001d)
    }

    "verifyTotalProbabilityEnglishFirstLetterFrequencies" in {
      LetterFrequency.englishFirstLetterFrequencies.values.sum must beCloseTo(1.0d, 0.0001d)
    }

    "verifyTotalProbabilityEnglishDoubleLetterFrequencies" in {
      LetterFrequency.englishDoubleLetterFrequencies.values.sum must beCloseTo(1.0d, 0.0001d)
    }

    "verifyLaplaceSmoothing" in {
      LetterFrequency.englishDoubleLetterFrequencies.get('x').get must beGreaterThan(0.0d)
    }

    "verifyGetBestMatchByLetterFrequency" in {
      val experimentalFrequency = 0.13
      'e' must_== LetterFrequency.getBestMatchByLetterFrequency(experimentalFrequency, Set()).get.letter
    }

    "verifyGetBestMatchByLetterFrequencyWithExclusion" in {
      val experimentalFrequency = 0.13
      't' must_== LetterFrequency.getBestMatchByLetterFrequency(experimentalFrequency, Set('e')).get.letter
    }

    "verifyGetBestMatchByFirstLetterFrequency" in {
      val experimentalFrequency = 0.17
      't' must_== LetterFrequency.getBestMatchByFirstLetterFrequency(experimentalFrequency, Set()).get.letter
    }

    "verifyGetBestMatchByFirstLetterFrequencyWithExclusion" in {
      val experimentalFrequency = 0.17
      'a' must_== LetterFrequency.getBestMatchByFirstLetterFrequency(experimentalFrequency, Set('t')).get.letter
    }

    "verifyGetBestMatchByDoubleLetterFrequency" in {
      val experimentalFrequency = 0.25
      's' must_== LetterFrequency.getBestMatchByDoubleLetterFrequency(experimentalFrequency, Set()).get.letter
    }

    "verifyGetBestMatchByDoubleLetterFrequencyWithExclusion" in {
      val experimentalFrequency = 0.25
      'l' must_== LetterFrequency.getBestMatchByDoubleLetterFrequency(experimentalFrequency, Set('s')).get.letter
    }
  }
}
