package org.bruchez.olivier.banktransactioncleanup

import java.nio.file.{Path, Paths}

object BankTransactionCleanup {
  def main(args: Array[String]): Unit = {
    //raiffeisenTest(Paths.get(args(0)))
    visecaTest(Paths.get(args(0)))
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
}
