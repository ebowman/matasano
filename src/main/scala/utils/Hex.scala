package utils

import utils.Types.HexString

object HexOps {

  implicit class ToHexOps(val str: String) extends AnyVal {
    def toHex: HexString = Hex.hexEncode(str)
  }

  implicit class FromHexOps(val str: HexString) extends AnyVal {
    def fromHex: String = Hex.hexDecode(str)
  }

  implicit class ToHexString(val bytes: Array[Byte]) extends AnyVal {
    def toHexString: HexString = {
      import Hex._
      bytes.flatMap(b => byteToHex(b.toInt)).mkString
    }
  }

  implicit class FromHexString(val bytes: HexString) extends AnyVal {
    def toBytes: Array[Byte] = {
      bytes.fromHex.getBytes("ISO-8859-1")
    }
  }

}

object Hex {

  val hexChars = ('0' to '9') ++ ('a' to 'f')

  def hexEncode(str: String): String = {
    str.map(_.toInt).flatMap(byteToHex).mkString
  }

  /** Convert like 255 into ff */
  def byteToHex(x: Int): String = {
    require(x >= 0 && x < 256, s"oops, x was $x")
    s"${nybbleToHex((x & 0xF0) >> 4)}${nybbleToHex(x & 0x0F)}"
  }

  // convert 0..15 to 0..F
  def nybbleToHex(x: Int): Char = {
    require(x >= 0 && x < 16)
    x match {
      case num if num >= 0 && num < 10 => (num + '0').toChar
      case _ => (x + 'a' - 10).toChar
    }
  }

  /** Given a string like "414243" returns "ABC" */
  def hexDecode(hexStr: String): String = {
    hexStr.grouped(2).map(hex2).map(_.toChar).mkString
  }

  // 2-char hex string to byte value
  def hex2(value: String): Int = {
    require(value.size == 2)
    hexCharToInt(value(0)) * 16 + hexCharToInt(value(1))
  }

  // '0' -> 'F' to 0..15
  def hexCharToInt(char: Character): Int = {
    char match {
      case num if num >= '0' && num <= '9' => num - '0'
      case num if num >= 'a' && num <= 'f' => num - 'a' + 10
      case num if num >= 'A' && num <= 'F' => num - 'A' + 10
      case unknown => throw new IllegalArgumentException(s"Unknown hex digit $unknown")
    }
  }

}
