package utils

object Base64 {
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
    hex.grouped(2).map(Hex.hex2).grouped(3).map(_.toArray).flatMap(toSixBits).map(table).mkString
  }
}
