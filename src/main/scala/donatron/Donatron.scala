package donatron

import cats.effect.Effect
import cats.implicits._
import donatron.models._

import scala.util.Try

class Donatron[F[_]: Effect]() {
  def donate(req: Request): F[Response] =
    checkForValidInts(req)
      .flatMap(checkForMinimumLength)
      .flatMap(submitDonations)
      .flatMap(logValidDonations)
      .flatMap(logAndReturnResponse)

  def checkForValidInts(req: Request): F[ValidIntsFound] = {
    val (validInts, nonInts) =
      req.values.partition(value => Try(value.toInt).isSuccess)

    validInts.isEmpty match {
      case true =>
        Effect[F].raiseError(new RuntimeException(NoValidInts(nonInts).show))
      case false =>
        Effect[F].pure(
          ValidIntsFound(validInts = validInts, invalidInts = nonInts)
        )
    }
  }

  def checkForMinimumLength(data: ValidIntsFound): F[IntsAboveMinimumFound] = {
    // We want to keep using the donations as string.
    // Hence, we can check that values are >= 10
    // by testing that length of value is > 1
    // ie., "9".length == 1, "10".length == > 2
    val (aboveMinimum, belowMinimum) = data.validInts.partition(_.length > 1)

    aboveMinimum.isEmpty match {
      case true => Effect[F].raiseError(new RuntimeException(data.show))
      case false =>
        Effect[F].pure(
          IntsAboveMinimumFound(
            aboveMinimum = aboveMinimum,
            lessThanMinimum = belowMinimum,
            invalidInts = data.invalidInts
          )
        )
    }
  }

  def submitDonations(data: IntsAboveMinimumFound): F[AcceptedDonations] = {
    checkForMaximumLength(data.aboveMinimum).map { validDonations =>
      AcceptedDonations(
        donations = validDonations,
        invalidInts = data.invalidInts,
        lessThanMinimum = data.lessThanMinimum
      )
    }
  }

  def checkForMaximumLength(data: List[String]): F[List[String]] =
    data.traverse { value =>
      value.length >= 5 match {
        case false => Effect[F].pure(s"Valid Number: $value")
        case true =>
          Effect[F].raiseError(
            new RuntimeException("Donation submission failed!")
          )
      }
    }

  def logAndReturnResponse(data: RawData): F[Response] =
    logResponse(data) >> Effect[F].delay(data.toResponse)

  def logResponse(data: RawData): F[Unit] =
    Effect[F].delay(println(s"Response: ${data.toLogMessage}"))

  def logValidDonations(donations: AcceptedDonations): F[AcceptedDonations] =
    Effect[F]
      .delay(println(s"Valid Donations: ${donations.toLogMessage}"))
      .map(_ => donations)
}
