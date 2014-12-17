package net.nomadicalien.nlp

import scala.concurrent.duration._

/**
 * Created by Shawn on 12/16/2014.
 */
package object actor {
  case object Start
  case object GeneratePerms
  case class Permutation(perm: List[Char])
  case object CompleteResult

  val mailbox = "bounded-mailbox"

  val numWorkers = 3

  val generationSchedule = 500 millis

  case class SlowDown()
}
