package utils

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class VectorSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "The Vector implementation" should "perform dot products" in {
    Vector.fromTerms(1, 0) dot Vector.fromTerms(0, 1) shouldEqual 0
    Vector.fromTerms(1, 0) dot Vector.fromTerms(1, 0) shouldEqual 1
    Vector.fromTerms(.5, .5) dot Vector.fromTerms(1, .5) shouldEqual 0.75
  }

  it should "allow constructing basis vectors" in {
    Vector.basis(3, 10, 5d) shouldEqual Vector(Seq(0,0,0,5d,0,0,0,0,0,0))
  }

  it should "support vector addition" in {
    Vector.fromTerms(0, 1, 0) + Vector.fromTerms(1, 0, 0) shouldEqual Vector.fromTerms(1, 1, 0)
  }

  it should "normalize correctly" in {
    Vector.fromTerms(1, 1, 1).normal shouldEqual Vector.fromTerms(1/math.sqrt(3), 1/math.sqrt(3), 1/math.sqrt(3))
  }
}
