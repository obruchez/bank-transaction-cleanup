package org.bruchez.olivier.banktransactioncleanup

import spire.math.Rational

import java.time.LocalDate

case class AccountStatement(amount: Rational,
                            currency: String,
                            bookingDate: LocalDate,
                            valueDate: LocalDate,
                            description: String,
                            source: String) {
  val valueYear: Int = valueDate.getYear
}

case object AccountStatement {
  def withoutNewlines(string: String): String = string.replaceAll("[\n\r]+", " / ")
}
