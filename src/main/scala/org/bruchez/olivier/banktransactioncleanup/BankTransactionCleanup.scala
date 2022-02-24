package org.bruchez.olivier.banktransactioncleanup

import java.nio.file.{Path, Paths}
import spire.implicits._

object BankTransactionCleanup {
  def main(args: Array[String]): Unit = {
    val Year = 2021

    val raiffeisenAccountStatements =
      this.raiffeisenAccountStatements(Paths.get(args(0)),
                                       args.toSeq.tail.map(Paths.get(_)),
                                       year = Year)

    val credits = raiffeisenAccountStatements.accountStatements.map(_.amount).filter(_ < 0).qsum
    val debits = raiffeisenAccountStatements.accountStatements.map(_.amount).filter(_ >= 0).qsum

    println(s"Transactions: ${raiffeisenAccountStatements.accountStatements.size}")
    println(s"Total: ${raiffeisenAccountStatements.totalAmount.toDouble}")
    println(s"Credits: ${credits.toDouble}")
    println(s"Debits: ${debits.toDouble}")
  }

  def raiffeisenAccountStatements(raiffeisenXmlFile: Path,
                                  visecaExcelDirectories: Seq[Path],
                                  year: Int): RaiffeisenAccountStatements = {

    val raiffeisenAccountStatements = RaiffeisenAccountStatements.fromFile(raiffeisenXmlFile)

    @scala.annotation.tailrec
    def merge(raiffeisenAccountStatements: RaiffeisenAccountStatements,
              visecaExcelDirectories: List[Path]): RaiffeisenAccountStatements =
      visecaExcelDirectories match {
        case Nil => raiffeisenAccountStatements

        case visecaExcelDirectory :: remainingVisecaExcelDirectories =>
          val visecaAccountStatements = VisecaAccountStatements.fromDirectory(visecaExcelDirectory)
          val mergedRaiffeisenAccountStatements =
            raiffeisenAccountStatements
              .mergedWithVisecaAccountStatements(visecaAccountStatements)
              .filteredByYear(year)

          merge(mergedRaiffeisenAccountStatements, remainingVisecaExcelDirectories)
      }

    merge(raiffeisenAccountStatements, visecaExcelDirectories.toList)
  }
}
