package net.nomadicalien.nlp


class KnownWordsSpec extends NLPSpec {
  "this" should {
    "verifyFindWord" in {
      KnownWords.findWord("hello") should beTrue
    }

    "verifyNumberWordsOfSize" in {
     KnownWords.numberWordsOfSize(4) must_== 7414
    }
  }
}
