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

  it should "encrypt & decrypt a known text" in {
    val plainTextHex = "The quick red fox jumps over the lazy brown down".toHex
    val key = "my dear aunt sal".toHex
    val encryptedHex = AES.encryptECB(key, plainTextHex)
    val decryptedHex = AES.decryptECB(key, encryptedHex)
    decryptedHex shouldBe plainTextHex
  }
}
