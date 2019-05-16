package utils.parsing

import java.nio.ByteBuffer

trait DataFunc {

  def stringToByteArray = (str: String, buf: Long) => {
    val out = new Array[Byte](buf.intValue())
    val strArr = str.toCharArray
    for (i <- out.indices) {
      if (i < strArr.length) out(i) = strArr(i).toByte
      else out(i) = 0
    }
    out
  }

  def shortToByteArray = (value: Int) =>
    ByteBuffer
      .allocate(2)
      .putShort {
        if (value > Short.MaxValue) (value + Short.MinValue).shortValue()
        else value.shortValue()
      }
          .array()
          .reverse

  def intToByteArray = (value: Long) =>
    ByteBuffer
      .allocate(4)
      .putInt{
        if (value > Int.MaxValue) (value + Int.MinValue).intValue()
        else value.intValue()
      }
      .array()
      .reverse

  def byteToByteArray = (value: Byte) =>
    ByteBuffer
      .allocate(1)
      .put(value)
      .array()

  def readUShort: Array[Byte] => Int =
    (hi: Array[Byte]) => {
      val sh = (hi(1) << 8 | hi(0)).intValue()
      if (sh < 0) -1 * sh + Short.MaxValue
      else sh
    }

  def readUInteger: Array[Byte] =>
    Long = (hi: Array[Byte]) => {
    val in = ByteBuffer.wrap(hi.reverse).getLong()
    if (in < 0) -1 * in + Integer.MAX_VALUE
    else in
  }

  def readString: Array[Byte] => String = (hi: Array[Byte]) => {
    hi.filter(_ != 0).map { x =>
      if (x != 0)
        x.toChar
    }.mkString
  }
}
