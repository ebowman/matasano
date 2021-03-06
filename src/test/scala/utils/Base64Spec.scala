package utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Base64.Base64Ops
import utils.HexOps._

class Base64Spec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Base64" should "correctly encode 3 bytes into 4 6-bit values" in {
    import utils.Base64._
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

  it should "correctly encode a known piece of text" in {
    val plain = "Man is distinguished, not only by his reason, but by this singular passion from"
    val encoded = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbQ"
    plain.toHex.toBase64 shouldBe encoded
  }

  it should "correctly decode a known piece of text" in {
    val plain = "Man is distinguished, not only by his reason, but by this singular passion from"
    val encoded = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbQ"
    encoded.fromBase64.fromHex shouldBe plain
  }

  it should "fail on toSixBits if passed too big a chunk" in {
    an[IllegalArgumentException] should be thrownBy Base64.toSixBits(new Array[Int](5))
  }

  it should "encode and decode correctly a variety of strings" in {
    for (text <- Seq("a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa")) {
      text.toHex.toBase64.fromBase64.fromHex shouldBe text
    }
  }

  it should "decode the smallest possible base64 string correctly" in {
    "a=".fromBase64 shouldBe "68"
  }

  it should "encode and decode arbitrary strings" in {
    val genChar = Gen.choose(0, 255)
    val genChars = Gen.listOf(genChar)
    val genString = for {
      list <- genChars
    } yield list.map(_.toChar).mkString

    forAll(genString) {
      (s: String) =>
        s.toHex.toBase64.fromBase64.fromHex shouldBe s
    }
  }
}
