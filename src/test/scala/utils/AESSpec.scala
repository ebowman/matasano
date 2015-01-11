package utils

import org.scalatest.{Matchers, FlatSpec}
import utils.HexOps._

class AESSpec extends FlatSpec with Matchers {

  "AES" should "detect duplications" in {
    val string = "abcdefablm".toHex
    AES.hasDuplicateBlocks(2)(string) shouldBe true
  }
  it should "claim no duplications" in {
    val string = "abcdefghijklmnopqrstu".toHex
    AES.hasDuplicateBlocks(2)(string) shouldBe false
  }
}
