package org.bruchez.olivier.banktransactioncleanup

import org.apache.poi.ss.usermodel.{Cell, Row, WorkbookFactory}
import spire.math.Rational

import java.nio.file.Path
import java.time.LocalDate
import scala.jdk.CollectionConverters._
import scala.util.Try

case class VisecaExcelFile(file: Path) {
  def visecaAccountStatements: VisecaAccountStatements = {
    val workbook = WorkbookFactory.create(file.toFile)

    try {
      val sheet = workbook.getSheetAt(0)

      // @todo complete description using multiple lines

      val accountStatements = sheet.rowIterator.asScala.toSeq.flatMap(accountStatementFromRow)

      // The first statement is the payment from the previous month
      VisecaAccountStatements(file,
                              accountStatements.tail,
                              paymentFromPreviousMonth = -accountStatements.head.amount)

    } finally {
      workbook.close()
    }
  }

  private def cellsFromRow(row: Row): Seq[String] =
    row.cellIterator.asScala.map(stringFromCell).toSeq

  private def stringFromCell(cell: Cell): String =
    Try(cell.getStringCellValue).getOrElse(cell.getNumericCellValue.toString)

  private def accountStatementFromRow(row: Row): Option[AccountStatement] = {
    val DatesIndex = 0
    val DescriptionIndex = 2
    val AmountInChfIndex = 5

    // We actually parse Excel (XSLS) file generated from Viseca's PDF reports via pdftables.com. The separation
    // into cells is not perfect, so we support two formats to cover all cases.

    val DatesAndDescriptionPattern = """.*(\d\d)\.(\d\d)\.(\d\d) (\d\d)\.(\d\d)\.(\d\d) (.+)""".r
    val DatesPattern = """(\d\d)\.(\d\d)\.(\d\d) (\d\d)\.(\d\d)\.(\d\d)""".r

    val cells = cellsFromRow(row)

    if (cells.size > AmountInChfIndex) {
      val Currency = "CHF"

      AccountStatement.withoutNewlines(cells(DatesIndex)) match {
        case DatesAndDescriptionPattern(valueDay,
                                        valueMonth,
                                        valueYear,
                                        bookingDay,
                                        bookingMonth,
                                        bookingYear,
                                        description) =>
          Some(
            AccountStatement(
              amountFromString(cells.last),
              currency = Currency,
              bookingDate = localDateFromStrings(bookingDay, bookingMonth, bookingYear),
              valueDate = localDateFromStrings(valueDay, valueMonth, valueYear),
              description = description
            ))

        case DatesPattern(valueDay, valueMonth, valueYear, bookingDay, bookingMonth, bookingYear) =>
          Some(
            AccountStatement(
              amountFromString(cells.last),
              currency = Currency,
              bookingDate = localDateFromStrings(bookingDay, bookingMonth, bookingYear),
              valueDate = localDateFromStrings(valueDay, valueMonth, valueYear),
              description = AccountStatement.withoutNewlines(cells(DescriptionIndex))
            ))

        case _ =>
          None
      }
    } else {
      None
    }
  }

  private def amountFromString(string: String): Rational = {
    val normalized = string.replaceAll("'", "").trim

    if (normalized.endsWith("-")) -Rational(normalized.init.trim) else Rational(normalized)
  }

  private def localDateFromStrings(day: String, month: String, year: String): LocalDate = {
    val ReferenceYear = 2000
    LocalDate.of(ReferenceYear + year.toInt, month.toInt, day.toInt)
  }
}
