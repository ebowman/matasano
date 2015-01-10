package set1.challenge3

import org.scalatest.{Matchers, FlatSpec}

class singleByteXorCypherSpec extends FlatSpec with Matchers {

  "singleByteXorCypher" should "decode a hex string" in {
    singleByteXorCypher.decodeHexString("41424344") shouldBe "ABCD"
  }

  it should "solve the test string" in {
    val input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    singleByteXorCypher.decrypt(input) shouldBe "Cooking MC's like a pound of bacon"
  }
}
