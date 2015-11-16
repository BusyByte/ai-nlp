package net.nomadicalien.nlp

class ProbFormatterSpec extends NLPSpec {
   "this" should {
     "verifyFormat" in {
       val numberToFormat = 1.6666666666d
       "1.66667" must_== ProbFormatter.format(numberToFormat)
     }
   }
}
