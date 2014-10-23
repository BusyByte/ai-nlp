package net.nomadicalien.nlp

/**
 * Created by Shawn on 10/23/2014.
 */
class LowerCaseLetter(theChar: Char) {
  def toChar = theChar.toLower

  override def toString: String = toChar.toString
}

object LowerCaseLetter {
  def apply(theChar: Char) = new LowerCaseLetter(theChar)
}
