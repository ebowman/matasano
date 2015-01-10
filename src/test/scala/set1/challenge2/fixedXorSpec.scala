package set1.challenge2

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec}


class fixedXorSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import set1.challenge1.convertHexToBase64._
  import fixedXor._

  "The fixedOr implementation" should "xor hex digits correctly" in {
    val hexDigitGenerator = Gen.oneOf(('0' to '9') ++ ('a' to 'f'))
    forAll(hexDigitGenerator) { a => xorHexDigit(a, '0') shouldEqual a }
    forAll(hexDigitGenerator) { a => xorHexDigit(a, 'f') shouldEqual intToHexChar(15 - hexCharToInt(a)) }
  }

  it should "pass the given test case at http://cryptopals.com/sets/1/challenges/2/" in {
    val a = "1c0111001f010100061a024b53535009181c"
    val b = "686974207468652062756c6c277320657965"
    fixedXor(a, b) shouldEqual "746865206b696420646f6e277420706c6179"
  }
}
