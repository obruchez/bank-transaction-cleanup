package org.bruchez.olivier.banktransactioncleanup

import org.apache.poi.ss.usermodel.{Cell, Row, WorkbookFactory}
import spire.math.Rational

import java.nio.file.Path
import java.time.LocalDate
import scala.jdk.CollectionConverters._
import scala.util.Try

case class VisecaExcelFile(file: Path) {
  private val source: String = file.toFile.getParentFile.getName

  def visecaAccountStatements: VisecaAccountStatements = {
    val workbook = WorkbookFactory.create(file.toFile)

    try {
      val sheet = workbook.getSheetAt(0)

      // A row can be a statement, a extra description, or an ignored row
      val accountStatementsAndExtraDescriptions = sheet.rowIterator.asScala.toSeq.flatMap { row =>
        accountStatementFromRow(row) match {
          case Some(accountStatement) => Some(Left(accountStatement))
          case None =>
            extraDescriptionFromRow(row) match {
              case Some(extraDescription) => Some(Right(extraDescription))
              case None                   => None
            }
        }
      }

      val accountStatements = withExtraDescriptionsMergedIntoAccountStatements(
        accountStatementsAndExtraDescriptions)

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
        case DatesAndDescriptionPattern(bookingDay,
                                        bookingMonth,
                                        bookingYear,
                                        valueDay,
                                        valueMonth,
                                        valueYear,
                                        description) =>
          Some(
            AccountStatement(
              amountFromString(cells.last),
              currency = Currency,
              bookingDate = localDateFromStrings(bookingDay, bookingMonth, bookingYear),
              valueDate = localDateFromStrings(valueDay, valueMonth, valueYear),
              description = description,
              source = source
            ))

        case DatesPattern(bookingDay, bookingMonth, bookingYear, valueDay, valueMonth, valueYear) =>
          Some(
            AccountStatement(
              amountFromString(cells.last),
              currency = Currency,
              bookingDate = localDateFromStrings(bookingDay, bookingMonth, bookingYear),
              valueDate = localDateFromStrings(valueDay, valueMonth, valueYear),
              description = AccountStatement.withoutNewlines(cells(DescriptionIndex)),
              source = source
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

  private def extraDescriptionFromRow(row: Row): Option[String] = {
    val ExtraDescriptionIndex = 2

    val cells = cellsFromRow(row)

    if (cells.size > ExtraDescriptionIndex) {
      val extraDescription = cells(ExtraDescriptionIndex)

      val otherCellsFromRow = cells.filter(_ != extraDescription)
      if (otherCellsFromRow.forall(_.isEmpty)) {
        Some(extraDescription)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def withExtraDescriptionsMergedIntoAccountStatements(
      accountStatementsAndExtraDescriptions: Seq[Either[AccountStatement, String]])
    : Seq[AccountStatement] = {
    @scala.annotation.tailrec
    def merged(remaining: List[Either[AccountStatement, String]],
               acc: List[AccountStatement] = Nil): Seq[AccountStatement] = {
      remaining match {
        case Nil => acc.reverse
        case head :: tail =>
          val (newRemaining, newAcc) =
            head match {
              case Left(accountStatement) =>
                // Take all following descriptions
                val extraDescriptions = tail.takeWhile(_.isRight).collect {
                  case Right(extraDescription) => extraDescription
                }

                // Merge descriptions
                val newAccountStatement = accountStatement.copy(
                  description = (accountStatement.description +: extraDescriptions).mkString(" / "))

                (tail.drop(extraDescriptions.size), newAccountStatement :: acc)

              case Right(_) =>
                // Just drop the description (no previous statement)
                (tail, acc)
            }

          merged(newRemaining, newAcc)
      }

    }

    merged(remaining = accountStatementsAndExtraDescriptions.toList)
  }
}
