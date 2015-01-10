package utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Hex._

class Base64Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  val digits: Seq[Char] = Seq('0' to '9', 'a' to 'f', 'A' to 'F').flatten
  val digitGen = Gen.oneOf(digits)
  val strGen = for {
    digit1 <- digitGen
    digit2 <- digitGen
  } yield s"$digit1$digit2"

  it should "correctly encode 3 bytes into 4 6-bit values" in {
    import Base64._
    val arrayGen = for {
      a <- Gen.choose(0, 255)
      b <- Gen.choose(0, 255)
      c <- Gen.choose(0, 255)
    } yield Array(a, b, c)

    forAll(arrayGen) {
      (bytes) =>
        fromSixBits(toSixBits(bytes)).toSeq shouldEqual bytes.toSeq
    }
  }
}
