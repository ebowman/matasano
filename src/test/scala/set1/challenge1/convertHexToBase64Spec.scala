package set1.challenge1

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class convertHexToBase64Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import set1.challenge1.convertHexToBase64._

  val digits: Seq[Char] = Seq('0' to '9', 'a' to 'f', 'A' to 'F').flatten
  val digitGen = Gen.oneOf(digits)
  val strGen = for {
    digit1 <- digitGen
    digit2 <- digitGen
  } yield s"$digit1$digit2"

  "The convertToBase64 functionality" should "convert the known test case" in {
    val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    convertHexToBase64(input) shouldEqual output
  }

  it should "correctly encode single hex digits" in {
    forAll(digitGen) {
      (digit: Char) =>
        hexDigit(digit) shouldEqual Character.digit(digit, 16)
    }
  }

  it should "fail encoding a non-hex char" in {
    an[IllegalArgumentException] should be thrownBy hexDigit('g')
  }

  it should "correctly encode 2-char hex strings" in {
    forAll(strGen) { case str =>
      hex2(str) shouldEqual Integer.parseInt(str, 16)
    }
  }

  it should "fail for long strings" in {
    an[IllegalArgumentException] should be thrownBy hex2("abc")
  }

  it should "correctly encode 3 bytes into 4 6-bit values" in {
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
