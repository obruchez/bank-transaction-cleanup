package org.bruchez.olivier.banktransactioncleanup

import spire.implicits._

import java.nio.file.{Path, Paths}

object BankTransactionCleanup {
  def main(args: Array[String]): Unit = {
    test(Paths.get(args(0)))
  }

  def test(path: Path): Unit = {
    val accountStatements = AccountStatement(path)
    println(accountStatements.mkString("\n"))

    val total = accountStatements.map(_.amount).qsum
    println(s"Total: ${total.toDouble}")
  }
}
