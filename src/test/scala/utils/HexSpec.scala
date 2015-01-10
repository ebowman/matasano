package utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Hex._

class HexSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  val digits: Seq[Char] = Seq('0' to '9', 'a' to 'f', 'A' to 'F').flatten
  val digitGen = Gen.oneOf(digits)
  val strGen = for {
    digit1 <- digitGen
    digit2 <- digitGen
  } yield s"$digit1$digit2"

  it should "correctly encode single hex digits" in {
    forAll(digitGen) {
      (digit: Char) =>
        hexCharToInt(digit) shouldEqual Character.digit(digit, 16)
    }
  }

  it should "fail encoding a non-hex char" in {
    an[IllegalArgumentException] should be thrownBy hexCharToInt('g')
  }

  it should "correctly encode 2-char hex strings" in {
    forAll(strGen) { case str =>
      hex2(str) shouldEqual Integer.parseInt(str, 16)
    }
  }

  it should "fail for long strings" in {
    an[IllegalArgumentException] should be thrownBy hex2("abc")
  }

  it should "decode a hex string" in {
    utils.Hex.hexDecode("41424344") shouldBe "ABCD"
  }

  it should "hex encode us-ascii strings" in {
    utils.Hex.hexEncode("abcd") shouldBe "61626364"
  }

  it should "throw an exception if the byteToHex argument is out of range" in {
    an[IllegalArgumentException] should be thrownBy Hex.byteToHex(256)
  }
}
