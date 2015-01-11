package utils

import java.util.zip.CRC32
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import utils.HexOps._
import utils.Types.HexString

object AES {
  def decrypt(key: HexString, cryptoHex: HexString): HexString = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    val keySpec = new SecretKeySpec(key.fromHex.getBytes("ISO-8859-1"), "AES")
    cipher.init(Cipher.DECRYPT_MODE, keySpec)
    val bytes = cipher.doFinal(cryptoHex.fromHex.getBytes("ISO-8859-1"))
    bytes.toHexString
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
