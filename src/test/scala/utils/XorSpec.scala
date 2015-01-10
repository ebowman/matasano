package utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}
import Xor._
import Hex._

class XorSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Xor" should "behave correctly" in {
    val hexDigitGenerator = Gen.oneOf(('0' to '9') ++ ('a' to 'f'))
    forAll(hexDigitGenerator) { a => xorHexDigit(a, '0') shouldEqual a }
    forAll(hexDigitGenerator) { a => xorHexDigit(a, 'f') shouldEqual intToHexChar(15 - hexCharToInt(a)) }
  }
}