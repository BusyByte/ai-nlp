package net.nomadicalien.nlp

class WordFrequencySpec extends NLPSpec {
   "this" should {
     "verifyEasy" in {
       val rank = WordFrequency.getRankingList(3).head
       "the" must_== rank.word
       1 must_== rank.rank
       WordFrequency.ONE_THIRD must_== rank.probability
     }

     "verifyEmpty" in {
       WordFrequency.getRankingList(200) must beEmpty
     }
   }
}
