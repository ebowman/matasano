package utils

object Base64 {

  implicit class Base64Ops(val str: String) extends AnyVal {
    def toBase64: String = encode(str)

    def fromBase64: String = decode(str)
  }

  // 3-byte array of 8-bit values to 4-byte array of 6-bit values
  def toSixBits(x: Array[Int]): Array[Int] = {
    require(x.size <= 3, s"oops: ${x.toSeq}")
    require(x.forall(x => x >= 0 && x < 256))
    x.size match {
      case 3 =>
        Array(
          // 111111(00)
          (x(0) & 0xFC) >> 2,
          // (000000)11 1111(0000)
          ((x(0) & 0x03) << 4) + ((x(1) & 0xF0) >> 4),
          // (0000)1111 11(000000)
          ((x(1) & 0x0F) << 2) + ((x(2) & 0xC0) >> 6),
          // (00)111111
          x(2) & 0x3F)
      case 2 =>
        Array(
          // 111111(00)
          (x(0) & 0xFC) >> 2,
          // (000000)11 1111(0000)
          ((x(0) & 0x03) << 4) + ((x(1) & 0xF0) >> 4),
          // (0000)1111 11(000000)
          (x(1) & 0x0F) << 2)
      case 1 =>
        Array(
          // 111111(00)
          (x(0) & 0xFC) >> 2,
          // (000000)11 1111(0000)
          (x(0) & 0x03) << 4)
    }
  }

  // 4-byte array of 6-bit values to 3-byte array of 8-bit values
  def fromSixBits(x: Array[Int]): Array[Int] = {
    require(x.size <= 4)
    // require(x.forall(table.map(_.toInt).filterNot(_ == 0).toSet),s"x ${x.toSeq} not in ${table.map(_.toInt).toSeq}")
    x.size match {
      case 4 =>
        Array(
          // (00)111111 (00)11(0000)
          (x(0) << 2) + (x(1) >> 4),
          // (0000)1111 (00)1111(00)
          ((x(1) & 0x0F) << 4) + ((x(2) & 0x3C) >> 2),
          // (000000)11 (00)111111
          ((x(2) & 0x03) << 6) + x(3)
        )
      case 3 =>
        Array(
          // (00)111111 (00)11(0000)
          (x(0) << 2) + (x(1) >> 4),
          // (0000)1111 (00)1111(00)
          ((x(1) & 0x0F) << 4) + ((x(2) & 0x3C) >> 2))
      case 2 =>
        Array(
          // (00)111111 (00)11(0000)
          (x(0) << 2) + (x(1) >> 4))
      case 1 =>
        Array(
          // (00)111111 (00)11(0000)
          x(0) << 2)
    }
  }

  // convert 0..63 to its base64 char value
  val table: Array[Char] = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Seq('+', '/')).toArray

  // convert a base64 char value to its 0..63 value
  lazy val reverseTable = {
    val array = new Array[Int](table.max + 1)
    table.zipWithIndex.foreach {
      case (c, i) => array(c) = i
    }
    array
  }

  /** convert a hex string to a base-64 encoded string */
  def encode(hex: String): String = {
    hex.grouped(2).map(Hex.hex2).grouped(3).map(_.toArray).flatMap(toSixBits).map(table).mkString
  }

  /**
   * decodes a base-64 encoded string into a hex string
   * so every 4 chars converts into 3 bytes or 6 hex chars
   */
  def decode(encoded: String): String = {
    encoded.takeWhile(_ != '=').grouped(4).flatMap { groupOf4 =>
      fromSixBits(groupOf4.map(_.toInt).map(reverseTable).toArray)
    }.flatMap(Hex.byteToHex).toSeq.mkString
  }
}
