package utils

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
