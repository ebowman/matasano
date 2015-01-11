package utils

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Types.HexString

class PaddingSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  val genBlockSize = Gen.choose(1, 255)
  val genHexByte = Gen.choose(0, 255).map(Hex.byteToHex)
  val genHexString = Gen.listOf(genHexByte).map(_.mkString: HexString)

  "Padding" should "correctly PKS#7 pad" in {
    forAll(genHexString, genBlockSize) {
      case (hexString, blockSize) =>
        val padded = Padding.pks7Pad(hexString, blockSize)
        ((padded.size / 2) % blockSize) shouldBe 0
        if (padded.size != hexString.size) {
          (Hex.hex2(padded.reverse.take(2).reverse) + hexString.size / 2) shouldBe padded.size/2
        } else {
          padded shouldBe hexString
        }
    }
  }
}
