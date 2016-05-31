package net.nomadicalien.nlp.actor

import net.nomadicalien.nlp.Sentence

case object RegisterWorker
case object WorkDone
case object SendMoreWork
case object Start
case class Permutation(perm: List[Char])
case class CompleteResult()
case class NewMax(sentence: Sentence)
