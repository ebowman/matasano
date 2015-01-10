package utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Hex._

class Base64Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

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
