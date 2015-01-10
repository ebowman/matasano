package utils

import utils.Hex.byteToHex

object RepeatingKeyXor {
  def encrypt(clearText: String, key: String): String = {
    val fullKey = key * (1 + clearText.size / key.size)
    clearText.zip(fullKey).map {
      case (c, k) => byteToHex(c ^ k)
    }.mkString
  }
}
