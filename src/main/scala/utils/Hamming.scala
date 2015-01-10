package utils

object Hamming {

  def distanceBytes(a: Int, b: Int): Int = {
    require(a >= 0 && a < 256)
    require(b >= 0 && b < 256)
    val xor = a ^ b
    (0 until 8).fold(0) {
      case (d, bit) => if ((xor & (1 << bit)) != 0) d + 1 else d
    }
  }
  def distance(a: String, b: String): Int = {
    a.zip(b).map(ab => Hamming.distanceBytes(ab._1, ab._2)).sum
  }
}
