package set1.challenge3

import org.scalatest.{FlatSpec, Matchers}

class CharFrequencyAnalyzerSpec extends FlatSpec with Matchers {

  "The CharAnalyzer implementation" should "handle some known cases correctly in" in {
    val sentence = "Now! is the ... time for all good men to come to the aid of their country?"
    val charScores = CharFrequencyAnalyzer.analyze(sentence)
    for (letter <- ('a' to 'z') :+ ' ') {
      val count = sentence.toLowerCase.count(_ == letter)
      if (letter == ' ') charScores.terms(26) shouldBe count
      else charScores.terms(letter - 'a') shouldBe count
    }
  }

  it should "load the corpus and give reasonable normalization" in {
    val freqDict: Map[Char, Double] = ('a' to 'z').zip(CharFrequencyAnalyzer.englishFreqVector.terms).toMap
    val sorted = freqDict.toSeq.sortBy(_._2)
    // the most and least common, as taken from
    // http://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language
    sorted.reverse.take(5).map(_._1).toSet shouldBe Set('a', 'e', 'i', 'o', 't')
    sorted.take(5).map(_._1).toSet shouldBe Set('z', 'q', 'j', 'x', 'k')
  }

}
