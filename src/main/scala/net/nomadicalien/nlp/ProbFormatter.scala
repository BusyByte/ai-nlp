package net.nomadicalien.nlp

import java.text.NumberFormat

/**
 * User: Shawn
 * Created: 4/11/13 10:30 PM
 */
object ProbFormatter {
 val formatHolder = new ThreadLocal[NumberFormat] {
   override def initialValue(): NumberFormat = {
      val numberFormat = NumberFormat.getInstance()
      numberFormat.setMinimumFractionDigits(5)
      numberFormat
   }
 }

  def format(probability : Double) = formatHolder.get().format(probability)
}
