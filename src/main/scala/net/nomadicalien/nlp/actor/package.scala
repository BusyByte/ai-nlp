package net.nomadicalien.nlp

/**
 * Created by Shawn on 12/16/2014.
 */
package object actor {
  case object Start
  case object GeneratePerms
  case class Permutation(perm: List[Char])
  case object CompleteResult

  val mailbox = "bounded-mailbox"
}
