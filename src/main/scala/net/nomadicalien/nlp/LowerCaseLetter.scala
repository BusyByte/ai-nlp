package net.nomadicalien.nlp

/**
 * Created by Shawn on 10/23/2014.
 */
case class LowerCaseLetter(theChar: Char) {
  def toChar = theChar.toLower

  override def toString: String = toChar.toString
}

