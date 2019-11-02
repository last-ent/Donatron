package donatron

import donatron.models._
import org.scalatest.{EitherValues, Matchers, WordSpec}

class DonatronSpec extends WordSpec with Matchers with EitherValues {

  val donatron: Donatron = new Donatron

  def getResponse(req: Request): Response =
    donatron.donate(req).unsafeRunSync()

  def getExceptionMessage(req: Request): String =
    donatron.donate(req).attempt.unsafeRunSync().left.value.getMessage

  "" should {

    "return Response message when Request has valid ints between 10 & 10k" in {
      val req = Request((10 to 20).map(_.toString).toList)

      val expectedResponse =
        AcceptedDonations(
          invalidInts = List.empty,
          lessThanMinimum = List.empty,
          donations = req.values.map(v => s"Valid Number: $v")
        ).toResponse

      getResponse(req) shouldEqual expectedResponse
    }

    "return Response message when Request has few invalid ints" in {
      val req = Request(List("10", "20", "10a", "asdf", "zzz124"))

      val expectedResponse =
        AcceptedDonations(
          invalidInts = req.values.drop(2),
          lessThanMinimum = List.empty,
          donations = req.values.take(2).map(v => s"Valid Number: $v")
        ).toResponse

      getResponse(req) shouldEqual expectedResponse
    }

    "return Response message when Request has few valid ints less than 10" in {
      val req = Request((7 to 20).map(_.toString).toList)

      val expectedResponse =
        AcceptedDonations(
          invalidInts = List.empty,
          lessThanMinimum = req.values.take(3),
          donations = req.values.drop(3).map(v => s"Valid Number: $v")
        ).toResponse

      getResponse(req) shouldEqual expectedResponse
    }

    "return NoValidInts message when Request has no valid ints" in {
      val req = Request(List("10a", "asdf", "zzz124"))

      getExceptionMessage(req) shouldEqual NoValidInts(invalidInts = req.values).toLogMessage.value
    }

    "return NoValuesAboveMinimum message when Request only has valid ints less than 10" in {
      val req = Request(List("1", "2"))

      val expectedResponse =
        NoValuesAboveMinimum(
          invalidInts = List.empty,
          lessThanMinimum = req.values
        )

      getExceptionMessage(req)  shouldEqual expectedResponse.toLogMessage.value
    }

    "return Exception when Request has valid ints greater than 10k" in {
      val req = Request((9999 to 10000).map(_.toString).toList)

      getExceptionMessage(req) shouldEqual "Failed to submit donations!"
    }
  }
}
