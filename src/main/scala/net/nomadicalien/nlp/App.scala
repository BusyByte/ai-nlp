package net.nomadicalien.nlp

import net.nomadicalien.nlp.actor._
import net.nomadicalien.nlp.actor.stream.genetic.ActorStreamGeneticNLP
import net.nomadicalien.nlp.actor.stream.{ActorStreamParrallelNLP, ActorStreamNLP}

object App extends Logging {
  val stringToDecode = "Esp qtcde nzyqpcpynp zy esp ezatn zq Lcetqtntlw Tyepwwtrpynp hld spwo le Olcexzfes Nzwwprp ty estd jplc."
  val solution = "The first conference on the topic of Artificial Intelligence was held at Dartmouth College in this year."
  val processors = List[NaturalLanguageProcessor](
    new BruteForceNLP(stringToDecode, solution),
    new ActorNLP(stringToDecode, solution),
    new ActorStreamNLP(stringToDecode, solution),
    new ActorStreamParrallelNLP(stringToDecode, solution),
    new ActorStreamGeneticNLP(stringToDecode, solution)
  )
    .map(nlp => nlp.getClass.getSimpleName -> nlp)
    .toMap

  val argsOptions = "Valid Options: " + processors.keys.mkString(",")

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
