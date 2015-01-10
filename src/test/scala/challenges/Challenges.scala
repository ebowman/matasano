package challenges

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.{RepeatingKeyXor, SingleByteXorCypher, Xor}

class Challenges extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "set 1 challenge 1" should "be solved in" in {
    val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    utils.Base64.encode(input) shouldEqual output
  }

  "set 1 challenge 2" should "be solved in" in {
    val a = "1c0111001f010100061a024b53535009181c"
    val b = "686974207468652062756c6c277320657965"
    Xor.xor(a, b) shouldEqual "746865206b696420646f6e277420706c6179"
  }

  "set 1 challenge 3" should "be solved in" in {
    val input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    SingleByteXorCypher.decrypt(input) shouldBe "Cooking MC's like a pound of bacon"
  }

  "set 1 challenge 4" should "be solved in" in {
    val lines = io.Source.fromInputStream(getClass.getResourceAsStream("/4.txt")).getLines()
    SingleByteXorCypher.findEncrypted(lines.toIterable) shouldBe "Now that the party is jumping\n"
  }

  "set 1 challenge 5" should "be solved in" in {
    val clearText = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    RepeatingKeyXor.encrypt(clearText, "ICE") shouldBe
      "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  }
}
