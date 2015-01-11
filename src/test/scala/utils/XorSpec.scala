package utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Hex._
import utils.Xor._

class XorSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Xor" should "behave correctly" in {
    val hexDigitGenerator = Gen.oneOf(('0' to '9') ++ ('a' to 'f'))
    forAll(hexDigitGenerator) { a => xorHexDigit(a, '0') shouldEqual a}
    forAll(hexDigitGenerator) { a => xorHexDigit(a, 'f') shouldEqual nybbleToHex(15 - hexCharToInt(a))}
  }
}
