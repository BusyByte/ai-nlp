package net.nomadicalien.nlp

/**
 * Created by Shawn on 10/23/2014.
 */
class LowerCaseLetter(theChar: Char) {
  val lowerCaseChar = theChar.toLower

  override def toString: String = lowerCaseChar.toString
}

