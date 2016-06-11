package net.nomadicalien.nlp


package object actor {
  val numWorkers = Config.getInt("akka.actor.default-dispatcher.fork-join-executor.parallelism-max").getOrElse(math.max(Runtime.getRuntime.availableProcessors() - 1, 1))
  val batchSize = 1000
}
