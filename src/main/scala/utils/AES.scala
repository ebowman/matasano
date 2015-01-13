package utils

import java.util.concurrent.ConcurrentHashMap
import java.util.zip.CRC32
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import utils.HexOps._
import utils.Types.HexString

object AES {

  def ecb(key: HexString, text: HexString, mode: Int): HexString = {
    require(key.size == 32)
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    val keySpec = new SecretKeySpec(key.fromHex.getBytes("ISO-8859-1"), "AES")
    cipher.init(mode, keySpec)
    val bytes = cipher.doFinal(text.fromHex.getBytes("ISO-8859-1"))
    bytes.toHexString
  }

  def decryptECB(key: HexString, cryptoHex: HexString): HexString = ecb(key, cryptoHex, Cipher.DECRYPT_MODE)

  def encryptECB(key: HexString, plainHex: HexString): HexString = ecb(key, plainHex, Cipher.ENCRYPT_MODE)

  def decryptCBC(key: HexString, cryptoHex: HexString, iv: HexString): HexString = {
    require(key.size == 32)
    val blocks = iv #:: cryptoHex.grouped(key.size).toStream
    (blocks.zip(blocks.tail) map { case (prev, cur) => Xor.xor(prev, decryptECB(key, cur)) }).mkString
  }

  def encryptCBC(key: HexString, cryptoHex: HexString, iv: HexString): HexString = {
    require(key.size == 32)
    require(iv.size == 32)
    require((cryptoHex.size % key.size) == 0)
    val blocks = cryptoHex.grouped(key.size).toStream
    blocks.foldLeft(iv :: Nil) {
      case (crypto, cur) => encryptECB(key, Xor.xor(crypto.head, cur)) :: crypto
    }.reverse.tail.mkString
  }

  def randomKey(size: Int = 16): HexString = {
    val bytes = new Array[Byte](size)
    scala.util.Random.nextBytes(bytes)
    bytes.toHexString
  }

  def encryptionOracle(plainText: HexString): HexString = {
    def randomBytes(): HexString =  randomKey(5 + util.Random.nextInt(6))
    val doctored = Padding.pks7Pad(randomBytes() + plainText + randomBytes(), 16)
    if (util.Random.nextBoolean()) {
      encryptECB(randomKey(), doctored)
    } else {
      encryptCBC(randomKey(), doctored, randomKey())
    }
  }

  def probablyECB(blockSize: Int)(data: HexString): Boolean = {
    data.toBytes.grouped(blockSize).map { group =>
      val crc = new CRC32
      crc.update(group)
      crc.getValue
    }.foldLeft((false, Set[Long]())) {
      case (a@(true, _), _) => a
      case ((false, set), crc) if set.contains(crc) => (true, set)
      case ((false, set), crc) => (false, set + crc)
    }._1
  }

  def ecbSuffixCracker(encrypter: HexString => HexString): String = {

    // compute a stream of the encryption block, each time encrypting one more
    // character.  zip that stream with its tail, so we can compute the difference
    // between each encryption and the one before it. As soon as the difference is
    // greater than 0, we know the block size. Note we divide by two because we are
    // working with HexStrings, which are 2 chars per byte.
    lazy val stream = Stream.from(0).map(i => encrypter(("A" * i).toHex).size)
    val blockSize = stream.zip(stream.tail).map(ab => (ab._2 - ab._1) / 2).dropWhile(_ == 0).head

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

    val numBlocks = encrypter("").size / (2 * blockSize)

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
          blocks(char) = thisBlock(encrypter(As + secret + char.toChar.toHex))
        }

        // encrypt with the right number of padded As, and then find the character that had to be
        // there in order to encrypt this block to that value. Then append that to the secret
        val toMatch = thisBlock(encrypter(As))
        val byte = blocks.find(_._2 == toMatch).map(_._1)
        secret + byte.map(_.toChar.toHex).getOrElse("") // if we can't find it, it's probably a padding byte; ignore
    }.fromHex
  }
}
