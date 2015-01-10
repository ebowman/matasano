package set1.challenge2

object fixedXor {

  import set1.challenge1.convertHexToBase64._

  def xorHexDigit(a: Char, b: Char): Char = intToHexChar(hexCharToInt(a) ^ hexCharToInt(b))

  def apply(hexStringA: String, hexStringB: String): String = {
    require(hexStringA.size == hexStringB.size)
    hexStringA.zip(hexStringB).map {
      case (a, b) => xorHexDigit(a, b)
    }.mkString
  }
}
