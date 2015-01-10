package set1.challenge3

import set1.challenge1.convertHexToBase64

/**
 * General purpose Vector class, for doing dot products and adding.
 */
case class Vector(terms: Seq[Double]) {

  def dot(other: Vector): Double = {
    require(terms.size == other.terms.size)
    terms.zip(other.terms).map(xy => xy._1 * xy._2).sum
  }

  def +(other: Vector): Vector = Vector(terms.zip(other.terms).map(xy => xy._1 + xy._2))
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

object CharFrequencyAnalyzer {

  /** Generate a vector of the frequence of each letter (and spaces). */
  def analyze(words: String): Vector = {
    val counts = new Array[Double](27)
    words.toLowerCase.foreach {
      case letter if letter >= 'a' && letter <= 'z' => counts(letter - 'a') += 1
      case ' ' => counts(26) += 1
      case ignore =>
    }
    Vector(counts)
  }

  /** Load an english corpus and compute the vector of frequencies for the english language. */
  lazy val englishFreqVector: Vector = {
    val source = io.Source.fromInputStream(getClass.getResourceAsStream("/eng_news_2005_10K-sentences.txt"))
    source.getLines().foldLeft(Vector.empty(27)) {
      case (v, line) => v + analyze(line)
    }
  }
}

object singleByteXorCypher {

  /** Given a string like "414243" returns "ABC" */
  def decodeHexString(hexStr: String): String = {
    hexStr.grouped(2).map(convertHexToBase64.hex2).map(_.toChar).mkString
  }

  /** Given a plaintext that has been encrypted by xoring it with a single char then hex encoded, try to decrypt it. */
  def decrypt(input: String): String = {
    // The assumption here (which seems to work in this case), is that if we have
    // a frequency vector for "english", then the best guess has the highest value when we take the dot product
    // between "english" and the analyzed frequency for an attempted decryption
    val engFreqs = CharFrequencyAnalyzer.englishFreqVector
    val decoded = decodeHexString(input)
    val results = for (x <- ('A' to 'Z') ++ ('a' to 'z')) yield {
      val candidate = decoded.map(c => c ^ x).map(_.toChar).mkString
      val freqs = CharFrequencyAnalyzer.analyze(candidate)
      (candidate, freqs dot engFreqs)
    }
    results.sortBy(_._2).reverse.head._1
  }
}
