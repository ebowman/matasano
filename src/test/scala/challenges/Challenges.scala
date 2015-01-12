package challenges

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import utils.Base64.Base64Ops
import utils.HexOps._
import utils.Types.HexString
import utils._

class Challenges extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "set 1 challenge 1" should "be solved" in {
    val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    utils.Base64.encode(input) shouldEqual output
  }

  "set 1 challenge 2" should "be solved" in {
    val a = "1c0111001f010100061a024b53535009181c"
    val b = "686974207468652062756c6c277320657965"
    Xor.xor(a, b) shouldEqual "746865206b696420646f6e277420706c6179"
  }

  "set 1 challenge 3" should "be solved" in {
    val input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    SingleByteXorCypher.crack(input)._2 shouldBe "Cooking MC's like a pound of bacon"
  }

  "set 1 challenge 4" should "be solved" in {
    val lines = io.Source.fromInputStream(getClass.getResourceAsStream("/4.txt")).getLines()
    SingleByteXorCypher.findEncrypted(lines.toIterable) shouldBe "Now that the party is jumping\n"
  }

  "set 1 challenge 5" should "be solved" in {
    val clearText = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val hexCrypto = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    RepeatingKeyXor.encrypt(clearText, "ICE") shouldBe hexCrypto
    RepeatingKeyXor.encrypt(hexCrypto.fromHex, "ICE").fromHex shouldBe clearText
    RepeatingKeyXor.crackRepeating(hexCrypto, 5) shouldBe "ICE"
  }

  "set 1 challenge 6" should "be solved" in {
    val hexCrypto = io.Source.fromInputStream(getClass.getResourceAsStream("/6.txt")).getLines().mkString.fromBase64
    val key = RepeatingKeyXor.crackRepeating(hexCrypto)
    key shouldBe "Terminator X: Bring the noise"
  }

  "set 1 challenge 7" should "be solved" in {
    val key = "YELLOW SUBMARINE"
    val hexCrypto = io.Source.fromInputStream(getClass.getResourceAsStream("/7.txt")).getLines().mkString.fromBase64
    AES.decryptECB(key.toHex, hexCrypto).fromHex should startWith("I'm back and I'm ringin' the bell")
  }

  "set 1 challenge 8" should "be solved" in {
    val cryptoLines = io.Source.fromInputStream(getClass.getResourceAsStream("/8.txt")).getLines()
    cryptoLines.exists(AES.probablyECB(16))
  }

  "set 2 challenge 10" should "be solved" in {
    val hexCrypto = io.Source.fromInputStream(getClass.getResourceAsStream("/10.txt")).getLines().mkString.fromBase64
    AES.decryptCBC("YELLOW SUBMARINE".toHex, hexCrypto, "00" * 16) should
      endWith("Play that funky music \n\u0004\u0004\u0004\u0004".toHex)
  }

  "set 2 challenge 11" should "be solved" in {
    // take a known plain text (which surely has some duplication)
    // encrypt it with a random key, a random iv, a random algorithm, and some garbage 100 times
    // try to detect whether it is ECB or CBC by detecting duplicate blocks
    // Since it's 50/50 whether we use ECB or CBC, it should be approximately 50/50 we detect one vs the other.
    val hexCrypto = io.Source.fromInputStream(getClass.getResourceAsStream("/10.txt")).getLines().mkString.fromBase64
    val hexText = AES.decryptCBC("YELLOW SUBMARINE".toHex, hexCrypto, "00" * 16)
    val ecbCounts = new AtomicInteger(0)
    val cbcCounts = new AtomicInteger(0)
    val count = 500
    (1 to count).par.foreach { _ =>
      val encrypted = AES.encryptionOracle(hexText)
      if (AES.probablyECB(16)(encrypted)) ecbCounts.incrementAndGet()
      else cbcCounts.incrementAndGet()
    }
    // this will occasionally fail... :-/
    ecbCounts.get() / count.toDouble shouldBe 0.5 +- 0.1
    cbcCounts.get() / count.toDouble shouldBe 0.5 +- 0.1
  }

  "set 2 challenge 12" should "be solved" in {
    val key = {
      def randomKey(size: Int = 16) = {
        val key = new Array[Byte](size)
        util.Random.nextBytes(key)
        key.toHexString
      }
      randomKey()
    }
    def encrypt(known: HexString): HexString = {
      import utils.Base64.Base64Ops
      val unknown: HexString = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK".fromBase64
      AES.encryptECB(key, Padding.pks7Pad(known + unknown, 16))
    }

    // 1.
    // compute a stream of the encryption block, each time encrypting one more
    // character.  zip that stream with its tail, so we can compute the difference
    // between each encryption and the one before it. As soon as the difference is
    // greater than 0, we know the block size. Note we divide by two because we are
    // working with HexStrings, which are 2 chars per byte.
    lazy val stream = Stream.from(0).map(i => encrypt(("A" * i).toHex).size)
    val blockSize = stream.zip(stream.tail).map(ab => (ab._2 - ab._1) / 2).dropWhile(_ == 0).head
    blockSize shouldBe 16

    // 2. As we introduce duplication into the encryption we should see duplicate blocks because, ECB
    AES.probablyECB(blockSize)(encrypt("A".toHex * 50)) shouldBe true
    // just to sanity check ECB vs CBC
    AES.probablyECB(blockSize)(AES.encryptECB("?".toHex * blockSize, "A".toHex * 32)) shouldBe true
    AES.probablyECB(blockSize)(AES.encryptCBC("?".toHex * blockSize, "A".toHex * 32, "*".toHex * blockSize)) shouldBe false


    // 3.
    // our first task is to crack the first letter of the secret. We do that
    // by padding the block with As leaving only the last character untouched.
    // Then we build a table of every possible character in that last spot,
    // and compare, which character in that spot produces the entire block that
    // the secret (with the As prepended) encrypted to
    val blockMinus1Encrypted = encrypt(("A" * (blockSize - 1)).toHex).take(2 * blockSize)

    (for (c <- (0 to 255).par) yield {
      c.toChar.toHex -> encrypt(("A" * (blockSize - 1)).toHex + c.toChar.toHex).take(2 * blockSize)
    }).find(_._2 == blockMinus1Encrypted).map(_._1) shouldBe Some("R".toHex)

    // 4.
    // Now let's find the entire secret. We can go block by block as follows. Imagine that
    // the block size is 4, and the secret is "secret".  Then we examine a procession of:
    //    AAA?
    //    AAs?
    //    Ase?
    //    sec?
    //    AAAs ecr?
    //    AAse cre?
    //    Asec ret

    val numBlocks = encrypt("").size / (2 * blockSize)

    (0 until numBlocks * blockSize).foldLeft("") {
      case (secret, i) =>
        val curBlock = i / blockSize
        val curIndex = i - curBlock * blockSize

        // for this index into the current block, generate these number of "A"s
        val As = ("A" * (blockSize - (curIndex + 1))).toHex

        // from a hex string, extract the current block under consideration
        def thisBlock(str: String) = str.drop(2 * curBlock * blockSize).take(2 * blockSize)

        // figure out, for every character, what it encrypts to at this position within this block
        import scala.collection.JavaConverters._
        val blocks = new ConcurrentHashMap[Int, String]().asScala
        for (char <- (0 until 256).par) {
          blocks(char) = thisBlock(encrypt(As + secret + char.toChar.toHex))
        }

        // encrypt with the right number of padded As, and then find the character that had to be
        // there in order to encrypt this block to that value. Then append that to the secret
        val toMatch = thisBlock(encrypt(As))
        val byte = blocks.find(_._2 == toMatch).map(_._1)
        secret + byte.map(_.toChar.toHex).getOrElse("") // if we can't find it, it's probably a padding byte; ignore
    }.fromHex should startWith("Rollin' in my 5.0")
  }
}
