package donatron.models

import cats.Show
import cats.implicits._

sealed trait RawData {
  def toResponse: Response = Response(toLogMessage.value)

  def toLogMessage: LogMessage
}

sealed trait RawDataSuccess extends RawData

sealed trait RawDataErr extends RawData

case class NoValidInts(invalidInts: List[String]) extends RawDataErr {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class NoValuesAboveMinimum(invalidInts: List[String],
                                lessThanMinimum: List[String])
    extends RawDataErr {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class ValidDonationsFound(invalidInts: List[String], validInts: List[String])
    extends RawDataSuccess {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class DonationsAboveMinimumFound(invalidInts: List[String],
                                      lessThanMinimum: List[String],
                                      aboveMinimum: List[String])
    extends RawDataSuccess {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class AcceptedDonations(invalidInts: List[String],
                             lessThanMinimum: List[String],
                             donations: List[String])
    extends RawDataSuccess {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

object RawData {
  implicit val showNoValidInts: Show[NoValidInts] =
    Show.show(err1 => s"""
         |NoValidInts: ${err1.invalidInts.mkString(", ")}""".stripMargin)

  implicit val showValidIntsFound: Show[ValidDonationsFound] =
    Show.show(stage2Data => s"""
        |ValidInts: ${stage2Data.validInts.mkString(", ")}
        |InValidInts: ${stage2Data.invalidInts.mkString(", ")}
        |""".stripMargin)

  implicit val showNoValuesAboveMinimum: Show[NoValuesAboveMinimum] =
    Show.show(err2 => s"""
        |NoValidInts: ${err2.invalidInts.mkString(", ")}
        |NoValuesAboveMinimum: ${err2.lessThanMinimum.mkString(", ")}
        |""".stripMargin)

  implicit val showIntsAboveMinimumFound: Show[DonationsAboveMinimumFound] =
    Show.show(fd => s"""
        |IntsAboveMinimum: ${fd.aboveMinimum.mkString(", ")}
        |InValidInts: ${fd.invalidInts.mkString(", ")}
        |BelowMinimumInts: ${fd.lessThanMinimum.mkString(", ")}
        |""".stripMargin)

  implicit val showAcceptedDonations: Show[AcceptedDonations] =
    Show.show(reply => s"""
        |Accepted Donations: ${reply.donations.mkString(", ")}
        |""".stripMargin)
}
