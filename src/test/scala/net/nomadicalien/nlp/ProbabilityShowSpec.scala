package net.nomadicalien.nlp

import net.nomadicalien.nlp.Probability.ProbabilityShow.show

class ProbabilityShowSpec extends NLPSpec {
   "this" should {
     "verifyFormat" in {
       val numberToFormat = 1.6666666666d
       "1.66667" must_== show(numberToFormat)
     }
   }
}
