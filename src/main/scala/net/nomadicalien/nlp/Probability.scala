package net.nomadicalien.nlp

import java.text.NumberFormat

import cats.Show

object Probability {
  type Probability = Double

  implicit object ProbabilityShow extends Show[Probability] {
    private val formatHolder = new ThreadLocal[NumberFormat] {
      override def initialValue() : NumberFormat = {
        val numberFormat = NumberFormat.getInstance()
        numberFormat.setMinimumFractionDigits(5)
        numberFormat
      }
    }
    override def show(probability: Probability): String = formatHolder.get.format(probability)
  }
}

