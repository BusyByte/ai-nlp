package net.nomadicalien.nlp

/**
 * @author Shawn Garner
 */
object App extends Logging {
  def main(args : Array[String]) {
    val startTime = System.currentTimeMillis()
    val stringToDecode = "Esp qtcde nzyqpcpynp zy esp ezatn zq Lcetqtntlw Tyepwwtrpynp hld spwo le Olcexzfes Nzwwprp ty estd jplc."
    val solution = "The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year."
    val nlp2 = new NLP2(stringToDecode, solution)
    val maxProbSentence = nlp2.process()
    logger.info(s"Result is: $maxProbSentence")
    val endTime = System.currentTimeMillis()
    logger.info(s"took ${((endTime - startTime) / 1000)} seconds")
  }
}
