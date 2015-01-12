package utils

import org.scalatest.{Matchers, FlatSpec}
import utils.HexOps._

class AESSpec extends FlatSpec with Matchers {

  "AES" should "detect duplications" in {
    val string = "abcdefablm".toHex
    AES.probablyECB(2)(string) shouldBe true
  }
  it should "claim no duplications" in {
    val string = "abcdefghijklmnopqrstu".toHex
    AES.probablyECB(2)(string) shouldBe false
  }

  it should "encrypt & decrypt a known text using ECB" in {
    val plainTextHex = "The quick red fox jumps over the lazy brown down".toHex
    val key = "my dear aunt sal".toHex
    val encryptedHex = AES.encryptECB(key, plainTextHex)
    val decryptedHex = AES.decryptECB(key, encryptedHex)
    decryptedHex shouldBe plainTextHex
  }

  it should "encrypt & decrypt a known text using CBC" in {
    val plainTextHex = "The quick red fox jumps over the lazy brown down".toHex
    val key = "my dear aunt sal".toHex
    val iv = AES.randomKey()
    val encryptedHex = AES.encryptCBC(key, plainTextHex, iv)
    val decryptedHex = AES.decryptCBC(key, encryptedHex, iv)
    decryptedHex shouldBe plainTextHex
  }
}
