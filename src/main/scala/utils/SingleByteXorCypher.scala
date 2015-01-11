package utils

import utils.Hex.hexDecode
import utils.Types.HexString

import scala.collection.immutable.IndexedSeq

object SingleByteXorCypher {

  /**
   * Given a bunch of random hex strings, figures out which one is encrypted using a single-byte xor cypher
   * (for set 1 challenge 4)
   */
  def findEncrypted(lines: Iterable[HexString]): String = {

    // for each line, find the best decryption for it
    val pairs: Iterable[(String, String)] = lines.map { line =>
      (line, SingleByteXorCypher.crack(line)._2)
    }

    // score each best decryption relative to english
    val solutions: Iterable[(String, Double)] = pairs.map {
      case (line, decrypted) =>
        (decrypted, CharFrequencyAnalyzer.analyze(decrypted) dot CharFrequencyAnalyzer.englishFreqVector)
    }

    // of all the best decryptions, which one looks the most like english?
    solutions.toSeq.sortBy(_._2).reverse.head._1
  }

  /** Given a hex-encoded plaintext that has been encrypted by xoring it with a single char then hex encoded, try to decrypt it. */
  def crack(input: HexString): (Char, String) = {
    // The assumption here (which seems to work in this case), is that if we have
    // a frequency vector for "english", then the best guess has the highest value when we take the dot product
    // between "english" and the analyzed frequency for an attempted decryption
    val engFreqs = CharFrequencyAnalyzer.englishFreqVector
    val decoded = hexDecode(input)
    val results: IndexedSeq[(Int, String, Double)] = for (x <- 0 to 255) yield {
      val candidate = decoded.map(c => c ^ x).map(_.toChar).mkString
      val freqs = CharFrequencyAnalyzer.analyze(candidate)
      (x, candidate, freqs dot engFreqs)
    }
    results.sortBy(_._3).reverse.map(x => (x._1.toChar, x._2)).head
  }
}
