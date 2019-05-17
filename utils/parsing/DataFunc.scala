package utils.parsing

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ArrayBuffer

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
        if (value > Short.MaxValue) (value - Short.MaxValue + Short.MinValue).shortValue()
        else value.shortValue()
      }
          .array()
          .reverse

  def intToByteArray = (value: Long) =>
    ByteBuffer
      .allocate(4)
      .putInt{
        if (value > Int.MaxValue) (value - Int.MaxValue + Int.MinValue).intValue()
        else value.intValue()
      }
      .array()
      .reverse

  def byteToByteArray = (value: Short) =>
    ByteBuffer
      .allocate(1)
      .put{
        if(value > Byte.MaxValue) (value - Byte.MaxValue + Byte.MinValue).byteValue()
        else value.byteValue()
      }
      .array()

  def readUByte: Byte => Short =
    (hi: Byte) => {
      val sh = hi.shortValue()
      if (sh < 0) (sh + Byte.MaxValue).shortValue()
      else sh
    }

  def readUShort: Array[Byte] => Int =
    (hi: Array[Byte]) => {
      val sh = (hi(1) << 8 | hi(0)).intValue()
      if (sh < 0) sh + Short.MaxValue
      else sh
    }

  def readUInteger: Array[Byte] =>
    Long = (hi: Array[Byte]) => {
    val in = ByteBuffer.wrap(hi.reverse).getLong()
    if (in < 0) in + Int.MaxValue
    else in
  }

  def readString: Array[Byte] => String = (hi: Array[Byte]) => {
    byteArr(hi).map(x => x.toChar).mkString
  }

  def readUtfString = (hi: Array[Byte]) => {
    new String(byteArr(hi), StandardCharsets.UTF_8)
  }

  def byteArr(hi: Array[Byte]) = {
    val out = new ArrayBuffer[Byte]()
    hi.foreach(x =>
    if (x != 0) out += x)
    out.toArray
  }

}
