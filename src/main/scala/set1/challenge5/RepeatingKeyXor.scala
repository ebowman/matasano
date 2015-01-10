package set1.challenge5

object RepeatingKeyXor {

  def encrypt(clearText: String, key: String): String = {
    import set1.challenge1.convertHexToBase64.byteToHex
    val fullKey = key * (1 + clearText.size / key.size)
    clearText.zip(fullKey).map {
      case (c, k) => byteToHex(c ^ k)
    }.mkString
  }
}
