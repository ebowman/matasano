package utils

object Hex {
  val hexChars = ('0' to '9') ++ ('a' to 'f')

  // '0' -> 'F' to 0..15
  def hexCharToInt(char: Character): Int = {
    char match {
      case num if num >= '0' && num <= '9' => num - '0'
      case num if num >= 'a' && num <= 'f' => num - 'a' + 10
      case num if num >= 'A' && num <= 'F' => num - 'A' + 10
      case unknown => throw new IllegalArgumentException(s"Unknown hex digit $unknown")
    }
  }

  // convert a byte value to a 2-char hex string
  def byteToHex(x: Int): String = {
    require(x >= 0 && x < 256)
    s"${intToHexChar((x & 0xF0) >> 4)}${intToHexChar(x & 0x0F)}"
  }

  // convert 0..15 to 0..F
  def intToHexChar(x: Int): Char = {
    require(x >= 0 && x < 16)
    x match {
      case num if num >= 0 && num < 10 => (num + '0').toChar
      case _ => (x + 'a' - 10).toChar
    }
  }

  // 2-char hex string to byte value
  def hex2(value: String): Int = {
    require(value.size == 2)
    hexCharToInt(value(0)) * 16 + hexCharToInt(value(1))
  }

  /** Given a string like "414243" returns "ABC" */
  def decodeHexString(hexStr: String): String = {
    hexStr.grouped(2).map(hex2).map(_.toChar).mkString
  }
}
