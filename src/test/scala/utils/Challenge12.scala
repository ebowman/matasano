package utils

import org.scalatest.{Matchers, FlatSpec}
import utils.Types.HexString
import utils.Base64.Base64Ops
import utils.HexOps._

class Challenge12 extends FlatSpec with Matchers {
  val unknownBase64 = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
  val randomKey = AES.randomKey()

  def encrypt(knownHex: HexString): HexString = {
    AES.encryptECB(randomKey, Padding.pks7Pad(knownHex + unknownBase64.fromBase64, 16))
  }

  "ECB" should "be successfully attacked" in {
    val initialSize = encrypt("").size
    val blockSize = -(1 to 20).foldLeft(initialSize) {
      case (prevSize, i) =>
        if (prevSize < 0) prevSize
        else {
          val crypto = encrypt(("A" * i).toHex)
          if (crypto.size > prevSize) -(crypto.size - prevSize)
          else prevSize
        }
    } / 2

    // 1.
    blockSize shouldBe 16

    // 2.
    AES.hasDuplicateBlocks(blockSize)(encrypt(("hello world" * 20).toHex)) shouldBe true

    // 3.
    val firstEncByte = encrypt("A".toHex * (blockSize - 1)).drop(2*(blockSize - 1)).take(2)
    println(s"firstEncByte = $firstEncByte")

    val firstByteDictionary =
      (for { i <- 0 to 255 } yield {
        Hex.byteToHex(i) -> encrypt(("A" * (blockSize - 1) + i.toChar.toString).toHex).drop(2*(blockSize - 1)).take(2)
      }).toMap
    //println(firstByteDictionary.toSeq.sortBy(_._1).mkString("\n"))
    println(firstByteDictionary.filter(_._2 == firstEncByte))

  }
}
