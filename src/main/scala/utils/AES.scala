package utils

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

import utils.Types.HexString
import utils.HexOps._

object AES {
  def decrypt(key: HexString, cryptoHex: HexString): HexString = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    val keySpec = new SecretKeySpec(key.fromHex.getBytes("ISO-8859-1"), "AES")
    cipher.init(Cipher.DECRYPT_MODE, keySpec)
    val bytes = cipher.doFinal(cryptoHex.fromHex.getBytes("ISO-8859-1"))
    bytes.toHexString
  }
}
