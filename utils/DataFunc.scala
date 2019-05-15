package utils

import java.nio.ByteBuffer

object DataFunc {

  def shortToByteArray = (value:Short) =>
    ByteBuffer
      .allocate(2)
      .putShort(value)
      .array()
      .reverse

  def intToByteArray = (value : Int) =>
    ByteBuffer
      .allocate(4)
      .putInt(value)
      .array()
      .reverse

  def byteToByteArray = (value : Byte) =>
    ByteBuffer
      .allocate(1)
      .put(value)
      .array()

  def readShort: Array[Byte] =>
    Short = (hi:Array[Byte]) => (hi(1) << 8 | hi(0)).toShort

  def readInteger: Array[Byte] =>
    Int = (hi:Array[Byte]) => ByteBuffer.wrap(hi.reverse).getInt()

  def readString: Array[Byte] => String = (hi:Array[Byte]) => {
    hi.filter(_ != 0).map{x =>
      if (x != 0)
        x.toChar
    }.mkString
  }
}
