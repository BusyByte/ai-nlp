package net.nomadicalien.nlp

import net.nomadicalien.nlp.actor.ActorNLP

/**
 * @author Shawn Garner
 */
object App extends Logging {
  val argsOptions = "Valid Options: Brute Force, Actor"

  val stringToDecode = "Esp qtcde nzyqpcpynp zy esp ezatn zq Lcetqtntlw Tyepwwtrpynp hld spwo le Olcexzfes Nzwwprp ty estd jplc."
  val solution = "The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year."
  val processors = Map(
  "Brute Force" -> new BruteForceNLP(stringToDecode, solution),
  "Actor" -> new ActorNLP(stringToDecode, solution)
  )

  def main(args : Array[String]) {
    require(args.length == 1, argsOptions)

    val processor = processors.get(args(0))
    if(processor.isEmpty) {
      System.out.println(argsOptions)
      System.exit(1)
    }

    val startTime = System.currentTimeMillis()
    val maxProbSentence = processor.get.process()
    logger.info(s"Result is: $maxProbSentence")
    val endTime = System.currentTimeMillis()
    logger.info(s"took ${(endTime - startTime) / 1000} seconds")
  }
}
