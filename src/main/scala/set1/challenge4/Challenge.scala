package set1.challenge4

import set1.challenge3.CharFrequencyAnalyzer

object Challenge {

  import set1.challenge3.singleByteXorCypher

  def findEncrypted(lines: Iterable[String]): String = {
    val pairs = lines.map {line =>
      (line, singleByteXorCypher.decrypt(line))
    }

    // now, which decryption looks the most like english?
    val solutions: Iterable[(String, String, Double)] = pairs.map {
      case (line, decrypted) =>
        (line, decrypted, CharFrequencyAnalyzer.analyze(decrypted) dot CharFrequencyAnalyzer.englishFreqVector)
    }

    solutions.toSeq.sortBy(_._3).reverse.head._2
  }
}
