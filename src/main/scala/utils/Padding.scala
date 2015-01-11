package utils

import utils.Types.HexString

object Padding {

  def pks7Pad(block: HexString, blockSize: Int): HexString = {
    require(blockSize > 0 && blockSize < 256)
    val size = block.size / 2
    val slop = if ((size % blockSize) == 0) 0 else blockSize * (1 + size / blockSize) - size
    val padStr = Hex.byteToHex(slop)
    block + (padStr * slop)
  }
}
