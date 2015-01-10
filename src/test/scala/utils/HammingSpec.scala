package utils

import org.scalatest.{Matchers, FlatSpec}

class HammingSpec extends FlatSpec with Matchers {

  "Hamming" should "compute the known distance between two test strings" in {
    Hamming.distance("this is a test", "wokka wokka!!!")
  }

  it should "compute single-byte distances correctly" in {
    Hamming.distanceBytes(0, 1) shouldBe 1
    Hamming.distanceBytes(0xF0, 0x0F) shouldBe 8
  }
}
