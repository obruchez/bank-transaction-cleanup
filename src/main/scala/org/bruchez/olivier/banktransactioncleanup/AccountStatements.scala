package org.bruchez.olivier.banktransactioncleanup

import spire.implicits._
import spire.math.Rational

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
