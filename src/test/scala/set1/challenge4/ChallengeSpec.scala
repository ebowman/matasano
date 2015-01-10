package set1.challenge4

import org.scalatest.{Matchers, FlatSpec}

class ChallengeSpec extends FlatSpec with Matchers {

  "The Challenge" should "solve" in {
    val lines = io.Source.fromInputStream(getClass.getResourceAsStream("/4.txt")).getLines()

    Challenge.findEncrypted(lines.toIterable) shouldBe "Now that the party is jumping\n"
  }
}
