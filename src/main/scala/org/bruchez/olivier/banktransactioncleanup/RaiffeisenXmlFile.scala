package org.bruchez.olivier.banktransactioncleanup

import spire.implicits._
import spire.math.Rational

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.LocalDate
import scala.xml.XML

case class RaiffeisenXmlFile(file: Path) {
  private case class AccountStatementDetails(amount: Rational,
                                             currency: String,
                                             name: String,
                                             iban: Option[String])

  // Specs: https://www.credit-suisse.com/media/assets/microsite/docs/zv-migration/camt-05x-001-04-sps.pdf

  def raiffeisenAccountStatements: RaiffeisenAccountStatements = {
    val xmlString = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    val parentNode = XML.loadString(xmlString)

    RaiffeisenAccountStatements(
      file,
      (parentNode \ "BkToCstmrStmt" \ "Stmt" \ "Ntry").flatMap(accountStatements))
  }

  private def accountStatements(node: xml.Node): Seq[AccountStatement] = {
    val accountStatement = AccountStatement(
      amount = amountWithSign(node),
      currency = (node \ "Amt" \ "@Ccy").text,
      bookingDate = LocalDate.parse((node \ "BookgDt" \ "Dt").text),
      valueDate = LocalDate.parse((node \ "ValDt" \ "Dt").text),
      description = AccountStatement.withoutNewlines((node \ "AddtlNtryInf").text)
    )

    val entryDetails = for { details <- node \ "NtryDtls" \ "TxDtls" } yield
      AccountStatementDetails(
        amount = amountWithSign(details),
        currency = (details \ "Amt" \ "@Ccy").text,
        name = AccountStatement.withoutNewlines((details \ "RltdPties" \ "Cdtr" \ "Nm").text),
        iban =
          Option((details \ "RltdPties" \ "CdtrAcct" \ "Id" \ "IBAN").text.trim).filter(_.nonEmpty)
      )

    val sameCurrency = entryDetails.map(_.currency).toSet == Set(accountStatement.currency)

    // Allow currency mismatch if only one entry detail
    assert(entryDetails.size <= 1 || sameCurrency,
           s"Currency mismatch: $accountStatement vs $entryDetails")

    // Check that amounts match
    assert(!sameCurrency || entryDetails.isEmpty || entryDetails
             .map(_.amount)
             .qsum == accountStatement.amount,
           s"Amount mismatch: $accountStatement vs $entryDetails")

    if (entryDetails.size <= 1) {
      Seq(accountStatement)
    } else {
      entryDetails map { ed =>
        accountStatement.copy(amount = ed.amount,
                              currency = ed.currency,
                              description = ed.name + ed.iban.map(" (" + _ + ")").getOrElse(""))
      }
    }
  }

  private def amountWithSign(node: xml.Node): Rational = {
    val amount = Rational((node \ "Amt").text)
    val cdtDbtInd = (node \ "CdtDbtInd").text

    if (cdtDbtInd == "DBIT") {
      amount
    } else if (cdtDbtInd == "CRDT") {
      -amount
    } else {
      throw new Exception(s"Unexpected CdtDbtInd value: $cdtDbtInd")
    }
  }
}
