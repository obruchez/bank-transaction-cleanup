package org.bruchez.olivier.banktransactioncleanup

import spire.implicits._

import java.nio.file.{Path, Paths}

object BankTransactionCleanup {
  def main(args: Array[String]): Unit = {
    //raiffeisenTest(Paths.get(args(0)))
    visecaTest(Paths.get(args(0)))
  }

  def raiffeisenTest(path: Path): Unit = {
    val accountStatements = RaiffeisenXmlFile(path).accountStatements
    println(accountStatements.mkString("\n"))

    val total = accountStatements.map(_.amount).qsum
    println(s"Total: ${total.toDouble}")
  }

  def visecaTest(path: Path): Unit = {
    val accountStatements = VisecaExcelFile(path).accountStatements
    println(accountStatements.mkString("\n"))

    val total = accountStatements.map(_.amount).qsum
    println(s"Total: ${total.toDouble}")
  }
}
