package utils

/**
 * General purpose Vector class, for doing dot products and adding.
 */
case class Vector(terms: Seq[Double]) {

  def dot(other: Vector): Double = {
    require(terms.size == other.terms.size)
    terms.zip(other.terms).map(xy => xy._1 * xy._2).sum
  }

  def +(other: Vector): Vector = Vector(terms.zip(other.terms).map(xy => xy._1 + xy._2))

  def normal = {
    def square(x: Double) = x * x
    val magnitude = math.sqrt(terms.map(square).sum)
    copy(terms = terms.map(_ / magnitude))
  }
}

object Vector {

  def fromTerms(terms: Double*) = new Vector(terms.toSeq)

  def basis(axis: Int, of: Int, value: Double): Vector = {
    val terms = new Array[Double](of)
    terms(axis) = value
    Vector(terms)
  }

  def empty(dim: Int): Vector = basis(0, dim, 0d)
}
