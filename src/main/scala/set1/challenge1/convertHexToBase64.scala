package set1.challenge1

object convertHexToBase64 {

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

  // 3-byte array of 8-bit values to 4-byte array of 6-bit values
  def toSixBits(x: Array[Int]): Array[Int] = {
    require(x.size == 3)
    require(x.forall(x => x >= 0 && x < 256))
    Array(
      // 111111(00)
      (x(0) & 0xFC) >> 2,
      // (000000)11 1111(0000)
      ((x(0) & 0x03) << 4) + ((x(1) & 0xF0) >> 4),
      // (0000)1111 11(000000)
      ((x(1) & 0x0F) << 2) + ((x(2) & 0xC0) >> 6),
      // (00)111111
      x(2) & 0x3F)
  }

  // 4-byte array of 6-bit values to 3-byte array of 8-bit values
  def fromSixBits(x: Array[Int]): Array[Int] = {
    require(x.size == 4)
    require(x.forall(x => x >= 0 && x < 64))
    Array(
      // (00)111111 (00)11(0000)
      (x(0) << 2) + (x(1) >> 4),
      // (0000)1111 (00)1111(00)
      ((x(1) & 0x0F) << 4) + ((x(2) & 0x3C) >> 2),
      // (000000)11 (00)111111
      ((x(2) & 0x03) << 6) + x(3)
    )
  }

  // convert 0..63 to its base64 char value
  val table: Array[Char] = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Seq('+', '/')).toArray

  // convert a hex string to a base-64 encoded string
  def apply(hex: String): String = {
    hex.grouped(2).map(hex2).grouped(3).map(_.toArray).flatMap(toSixBits).map(table).mkString
  }
}

// vim: set ts=2 sw=2 et: