package donatron

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import donatron.models._

import scala.util.Try

class Donatron() {

  def donate(req: Request): IO[Response] =
    checkForValidIntsET(req)
      .flatMap(checkForMinimumDonationAmountET)
      .flatMap(submitDonationsET)
      .flatMap(logAndReturnAcceptedDonationsET)
      .merge
      .flatMap(logAndReturnResponse)

  def checkForValidIntsET(req: Request): EitherT[IO, RawData, ValidDonationsFound] = {
    val parts =
      req.values.partition(value => Try(value.toInt).isSuccess)

    EitherT.fromEither(
      parts match {
        case (noValidInts: List[String], allNonInts: List[String])
          if noValidInts.isEmpty =>
          Left(NoValidInts(invalidInts = allNonInts))

        case (validDonations: List[String], nonInts: List[String]) =>
          Right(ValidDonationsFound(invalidInts = nonInts, validInts = validDonations))
      }
    )
  }

  def checkForMinimumDonationAmountET(data: ValidDonationsFound): EitherT[IO, RawData, DonationsAboveMinimumFound] = {
    // We want to keep using the donations as string.
    // Hence, we can check that values are >= 10
    // by testing that length of value is > 1
    // ie., "9".length == 1, "10".length == > 2
    val parts = data.validInts.partition(_.length > 1)

    EitherT.fromEither(
      parts match {
        case (noneAboveMinimum: List[String], allBelowMinimum: List[String])
          if noneAboveMinimum.isEmpty =>
          Left(
            NoValuesAboveMinimum(
              invalidInts = data.invalidInts,
              lessThanMinimum = allBelowMinimum
            )
          )
        case (aboveMinimum: List[String], belowMinimum: List[String]) =>
          Right(
            DonationsAboveMinimumFound(
              invalidInts = data.invalidInts,
              lessThanMinimum = belowMinimum,
              aboveMinimum = aboveMinimum
            )
          )
      }
    )
  }

  def submitDonations(data: DonationsAboveMinimumFound): IO[AcceptedDonations] =
    checkForAboveMaxDonationAmount(data.aboveMinimum)
      .map { validDonations =>
        AcceptedDonations(
          donations = validDonations,
          invalidInts = data.invalidInts,
          lessThanMinimum = data.lessThanMinimum
        )
      }

  def submitDonationsET(data: DonationsAboveMinimumFound): EitherT[IO, RawData, AcceptedDonations] =
    EitherT.liftF(submitDonations(data))

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

  def logAndReturnAcceptedDonationsET(donations: AcceptedDonations): EitherT[IO, RawData, AcceptedDonations] =
    EitherT.liftF(logAndReturnAcceptedDonations(donations))
}
