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
    val checksums = data.toBytes.grouped(blockSize).map {
      group =>
        val crc = new CRC32
        crc.update(group)
        crc.getValue
    }.toSeq
    checksums.distinct.size != checksums.size
  }
}
