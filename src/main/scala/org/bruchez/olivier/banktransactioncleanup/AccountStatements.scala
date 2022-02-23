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
    extends AccountStatements

object RaiffeisenAccountStatements {
  def fromFile(file: Path): RaiffeisenAccountStatements = {
    val accountStatements = RaiffeisenXmlFile(file).accountStatements

    RaiffeisenAccountStatements(file, accountStatements)
  }
}

case class VisecaAccountStatements(override val file: Path,
                                   override val accountStatements: Seq[AccountStatement])
    extends AccountStatements

object VisecaAccountStatements {
  def fromDirectory(directory: Path): Seq[VisecaAccountStatements] = {
    directory.toFile.listFiles
      .filter(f => f.isFile && f.getName.endsWith(".xlsx"))
      .sortBy(_.getName) map { file =>
      val accountStatements = VisecaExcelFile(file.toPath).accountStatements

      VisecaAccountStatements(file.toPath, accountStatements)
    }
  }
}
