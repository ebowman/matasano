package utils

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

  def hasDuplicateBlocks(blockSize: Int)(data: HexString): Boolean = {
    data.toBytes.grouped(blockSize).map {
      group =>
        val crc = new CRC32
        crc.update(group)
        crc.getValue
    }.foldLeft((false, Set[Long]())) {
      case (a@(true, _), _) => a
      case ((false, set), crc) if set.contains(crc) => (true, set)
      case ((false, set), crc) => (false, set + crc)
    }._1
  }
}
