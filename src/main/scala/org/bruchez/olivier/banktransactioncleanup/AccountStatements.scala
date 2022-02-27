package org.bruchez.olivier.banktransactioncleanup

import org.apache.poi.xssf.usermodel.XSSFWorkbook
import spire.implicits._
import spire.math.Rational

import java.io.FileOutputStream
import java.nio.file.Path

sealed trait AccountStatements {
  def file: Path
  def accountStatements: Seq[AccountStatement]

  val totalAmount: Rational = accountStatements.map(_.amount).qsum
}

case class RaiffeisenAccountStatements(override val file: Path,
                                       override val accountStatements: Seq[AccountStatement])
    extends AccountStatements {
  def filteredByYear(year: Int): RaiffeisenAccountStatements = {
    this.copy(accountStatements = this.accountStatements.filter(_.valueYear == year))
  }

  def mergedWithVisecaAccountStatements(
      visecaAccountStatements: Seq[VisecaAccountStatements]): RaiffeisenAccountStatements = {

    val visecaAccountStatementsWithPaymentsFromNextMonth =
      for {
        (current, next) <- visecaAccountStatements.zip(
          visecaAccountStatements.tail.map(Some.apply) :+ None)
      } yield (current, next.map(_.paymentFromPreviousMonth))

    @scala.annotation.tailrec
    def merged(mergeDestination: RaiffeisenAccountStatements,
               mergeSources: List[(VisecaAccountStatements, Option[Rational])])
      : RaiffeisenAccountStatements =
      mergeSources match {
        case Nil =>
          mergeDestination

        case mergeSource :: remainingSources =>
          val mergedWithOne =
            mergeDestination.mergedWithVisecaAccountStatements(mergeSource._1, mergeSource._2)

          merged(mergedWithOne, remainingSources)
      }

    merged(this, visecaAccountStatementsWithPaymentsFromNextMonth.toList)
  }

  def exportToExcel(file: Path): Unit = {
    val workbook = new XSSFWorkbook
    val sheet = workbook.createSheet("Transactions")

    val headerRow = sheet.createRow(0)
    headerRow.createCell(0).setCellValue("Date")
    headerRow.createCell(1).setCellValue("Valeur")
    headerRow.createCell(2).setCellValue("Source")
    headerRow.createCell(3).setCellValue("Description")

    for { (accountStatement, index) <- this.accountStatements.zipWithIndex } {
      val row = sheet.createRow(index + 1)

      // Invert sign of amount for import into e.g. Money Pro
      row.createCell(0).setCellValue(accountStatement.valueDate)
      row.createCell(1).setCellValue(-accountStatement.amount.toDouble)
      row.createCell(2).setCellValue(accountStatement.source)
      row.createCell(3).setCellValue(accountStatement.description)
    }

    val outputStream = new FileOutputStream(file.toFile)

    try {
      workbook.write(outputStream)
    } finally {
      if (outputStream != null) {
        outputStream.close()
      }
    }
  }

  private def mergedWithVisecaAccountStatements(
      visecaAccountStatements: VisecaAccountStatements,
      paymentFromNextMonth: Option[Rational]): RaiffeisenAccountStatements = {
    paymentFromNextMonth match {
      case None =>
        this.copy(
          accountStatements = this.accountStatements ++ visecaAccountStatements.accountStatements)

      case Some(payment) =>
        // Look for the payment to Viseca; the payment amount is given in next month's report
        val statementsToReplace = this.accountStatements filter { as =>
          as.amount == payment && as.description.toLowerCase.contains("viseca")
        }

        assert(statementsToReplace.size == 1)

        val statementToReplace = statementsToReplace.head

        this.copy(
          accountStatements = this.accountStatements
            .filterNot(_ == statementToReplace) ++ visecaAccountStatements.accountStatements)
    }
  }
}

object RaiffeisenAccountStatements {
  def fromFile(file: Path): RaiffeisenAccountStatements =
    RaiffeisenXmlFile(file).raiffeisenAccountStatements
}

case class VisecaAccountStatements(override val file: Path,
                                   override val accountStatements: Seq[AccountStatement],
                                   paymentFromPreviousMonth: Rational)
    extends AccountStatements

object VisecaAccountStatements {
  def fromDirectory(directory: Path): Seq[VisecaAccountStatements] = {
    val excelFiles = directory.toFile.listFiles
      .filter(f => f.isFile && f.getName.endsWith(".xlsx"))
      .sortBy(_.getName)

    excelFiles.map(file => VisecaExcelFile(file.toPath).visecaAccountStatements)
  }
}
