package donatron

import cats.effect.IO
import cats.implicits._
import donatron.models._

import scala.util.Try

class Donatron() {
  def donate(req: Request): IO[Response] =
    checkForValidInts(req)
      .flatMap(checkForMinimumDonationAmount)
      .flatMap(submitDonations)
      .flatMap(logAndReturnAcceptedDonations)
      .flatMap(logAndReturnResponse)

  def checkForValidInts(req: Request): IO[ValidDonationsFound] = {
    val (validInts, nonInts) =
      req.values.partition(value => Try(value.toInt).isSuccess)

    validInts.isEmpty match {
      case true =>
        IO.raiseError(new RuntimeException(NoValidInts(nonInts).show))
      case false =>
        IO.pure(
          ValidDonationsFound(validInts = validInts, invalidInts = nonInts)
        )
    }
  }

  def checkForMinimumDonationAmount(data: ValidDonationsFound): IO[DonationsAboveMinimumFound] = {
    // We want to keep using the donations as string.
    // Hence, we can check that values are >= 10
    // by testing that length of value is > 1
    // ie., "9".length == 1, "10".length == > 2
    val (aboveMinimum, belowMinimum) = data.validInts.partition(_.length > 1)

    aboveMinimum.isEmpty match {
      case true => IO.raiseError(new RuntimeException(data.show))
      case false =>
        IO.pure(
          DonationsAboveMinimumFound(
            aboveMinimum = aboveMinimum,
            lessThanMinimum = belowMinimum,
            invalidInts = data.invalidInts
          )
        )
    }
  }

  def submitDonations(data: DonationsAboveMinimumFound): IO[AcceptedDonations] = {
    checkForAboveMaxDonationAmount(data.aboveMinimum).map { validDonations =>
      AcceptedDonations(
        donations = validDonations,
        invalidInts = data.invalidInts,
        lessThanMinimum = data.lessThanMinimum
      )
    }
  }

  def checkForAboveMaxDonationAmount(data: List[String]): IO[List[String]] =
    data.traverse { value =>
      value.length >= 5 match {
        case false => IO.pure(s"Valid Number: $value")
        case true =>
          IO.raiseError(
            new RuntimeException("Failed to submit donations!")
          )
      }
    }

  def logAndReturnResponse(data: RawData): IO[Response] =
    logResponse(data) >> IO.delay(data.toResponse)

  def logResponse(data: RawData): IO[Unit] =
    IO.delay(println(s"Response: ${data.toLogMessage}"))

  def logAndReturnAcceptedDonations(donations: AcceptedDonations): IO[AcceptedDonations] =
    IO
      .delay(println(s"Valid Donations: ${donations.toLogMessage}"))
      .map(_ => donations)
}
