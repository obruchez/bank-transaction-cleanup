package org.bruchez.olivier.banktransactioncleanup

import java.nio.file.{Path, Paths}

object BankTransactionCleanup {
  def main(args: Array[String]): Unit = {
    //raiffeisenTest(Paths.get(args(0)))
    //visecaTest(Paths.get(args(0)))
    mergeTest(Paths.get(args(0)), Paths.get(args(1)))
  }

  def raiffeisenTest(file: Path): Unit = {
    val raiffeisenAccountStatements = RaiffeisenAccountStatements.fromFile(file)

    println(raiffeisenAccountStatements.accountStatements.mkString("\n"))
    println(s"Total: ${raiffeisenAccountStatements.totalAmount.toDouble}")
  }

  def visecaTest(directory: Path): Unit =
    VisecaAccountStatements.fromDirectory(directory) foreach { visecaAccountStatements =>
      println(
        s"Total (${visecaAccountStatements.file.toFile.getName}): ${visecaAccountStatements.totalAmount.toDouble}")
    }

  def mergeTest(file: Path, directory: Path): Unit = {
    val raiffeisenAccountStatements = RaiffeisenAccountStatements.fromFile(file)
    val visecaAccountStatements = VisecaAccountStatements.fromDirectory(directory)

    println(s"Raiffeisen total (before merge): ${raiffeisenAccountStatements.totalAmount.toDouble}")

    val mergedRaiffeisenAccountStatements =
      raiffeisenAccountStatements.mergedWithVisecaAccountStatements(visecaAccountStatements)

    println(
      s"Raiffeisen total (after merge): ${mergedRaiffeisenAccountStatements.totalAmount.toDouble}")
  }
}
