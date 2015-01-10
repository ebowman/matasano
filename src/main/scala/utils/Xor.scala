package utils

object Xor {

  import utils.Hex._

  def xorHexDigit(a: Char, b: Char): Char = nybbleToHex(hexCharToInt(a) ^ hexCharToInt(b))

  def xor(hexStringA: String, hexStringB: String): String = {
    require(hexStringA.size == hexStringB.size)
    hexStringA.zip(hexStringB).map {
      case (a, b) => xorHexDigit(a, b)
    }.mkString
  }
}
