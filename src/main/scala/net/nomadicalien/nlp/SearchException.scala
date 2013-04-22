package net.nomadicalien.nlp

/**
 * User: Shawn Garner
 * Created: 4/21/13 7:15 PM
 */
abstract class SearchException(theMessage : String) extends Exception(theMessage)
case class OutOfOptionsException(theMessage : String) extends SearchException(theMessage)
case class CloseEnoughMatchException(theMessage : String) extends SearchException(theMessage)
