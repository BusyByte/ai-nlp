package net.nomadicalien.nlp

import scala.concurrent.duration._

/**
 * Created by Shawn on 12/16/2014.
 */
package object actor {
  case object RegisterWorker
  case object WorkDone
  case object SendMoreWork
  case object Start
  case class Permutation(perm: List[Char])
  case class CompleteResult()
  case class NewMax(sentence: Sentence)

  val numWorkers = 3
  val batchSize = 1000


}
