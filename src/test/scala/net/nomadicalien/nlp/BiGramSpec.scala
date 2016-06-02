package net.nomadicalien.nlp

class BiGramSpec extends NLPSpec {
  "this" should {
    "verifyProbOfCurrentGivenPriorTH" in {
      val probOfHGivenT = BiGram.probOfAGivenB('h', 't')
      probOfHGivenT must beCloseTo(0.3447d, 0.0001d)
    }

    "verifyProbOfCurrentGivenPriorHT" in {
      val probOfTGivenH = BiGram.probOfAGivenB('t', 'h')
      probOfTGivenH must beCloseTo(0.0341d, 0.0001d)
    }
  }

}
