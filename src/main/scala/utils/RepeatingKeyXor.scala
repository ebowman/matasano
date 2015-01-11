package utils

import utils.Hex.byteToHex

import scala.collection.mutable.ListBuffer

object RepeatingKeyXor {

  /** returns a hex string encrypted (or decrypted) version from the given key. */
  def encrypt(clearText: String, key: String): String = {
    val fullKey = key * (1 + clearText.size / key.size)
    clearText.zip(fullKey).flatMap {
      case (c, k) => byteToHex(c ^ k)
    }.mkString
  }

  /** given a hex-encoded crypto string, returns the key that will decrypt it */
  def crackRepeating(hexCrypto: String): String = {
    val cryptoText = Hex.hexDecode(hexCrypto)
    val distances = for (keySize <- 2 to 10) yield {
      val n = 4
      val samples = cryptoText.grouped(keySize).drop(1).take(n).toSeq
      val distances = samples.zip(samples.tail).map(s => Hamming.distance(s._1, s._2))
      val hammingDistance = distances.sum / (n - 1d)
      (keySize, hammingDistance)    // note we are NOT normalizing, despite the suggestion to do so
    }

    val bestKeysizes = distances.sortBy(_._2).take(3).map(_._1) // lowest hamming distances
    val keyCandidates = for (keySize <- bestKeysizes) yield {
      val buffers = new Array[ListBuffer[Char]](keySize)
      for (i <- 0 until keySize) buffers(i) = new ListBuffer[Char]

      cryptoText.grouped(keySize).foreach {
        (block: String) =>
          block.zipWithIndex.foreach {
            case (c: Char, i: Int) => buffers(i) += c
          }
      }

      buffers.map { buffer =>
        SingleByteXorCypher.crack(Hex.hexEncode(buffer.mkString))._1
      }.toSeq.mkString
    }

    keyCandidates.map { key =>
      val decrypted = Hex.hexDecode(RepeatingKeyXor.encrypt(cryptoText, key))
      val analyzed = CharFrequencyAnalyzer.analyze(decrypted)
      key -> analyzed.dot(CharFrequencyAnalyzer.englishFreqVector)
    }.sortBy(_._2).reverse.head._1
  }
}
