package utils.parsing

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ArrayBuffer

trait DataFunc {

  def stringToByteArray: (String, Long) => Array[Byte] = (str: String, buf: Long) => {
    val out = new Array[Byte](buf.intValue())
    val strArr = str.toCharArray
    for (i <- out.indices) {
      if (i < strArr.length) out(i) = {
        val c = strArr(i).toByte
        if (c.toByte == 'ё'.toByte) (- 72).byteValue()
        else if(c.toByte == 'Ё'.toByte) (-88).byteValue()
        else if (c.toByte >= 'а'.toByte && c.toByte <= 'е'.toByte) (c.toByte - 'а'.toByte- 32).byteValue()
        else if (c.toByte >= 'А'.toByte && c.toByte <= 'Е'.toByte) (c.toByte - 'А'.toByte- 64).byteValue()
        else if (c.toByte >= 'ж'.toByte && c.toByte <= 'я'.toByte) (c.toByte - 'ж'.toByte- 26).byteValue()
        else if (c.toByte >= 'Ж'.toByte && c.toByte <= 'Я'.toByte) (c.toByte - 'Ж'.toByte- 58).byteValue()
        else {c.toByte}
      }
      else out(i) = 0
    }
    out
  }

  def shortToByteArray: Int => Array[Byte] = (value: Int) =>
    ByteBuffer
      .allocate(2)
      .putShort {
        if (value > Short.MaxValue) (value - Short.MaxValue + Short.MinValue).shortValue()
        else value.shortValue()
      }
          .array()
          .reverse

  def intToByteArray: Long => Array[Byte] = (value: Long) =>
    ByteBuffer
      .allocate(4)
      .putInt{
        if (value > Int.MaxValue) (value - Int.MaxValue + Int.MinValue).intValue()
        else value.intValue()
      }
      .array()
      .reverse

  def byteToByteArray: Short => Array[Byte] = (value: Short) =>
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
    val in = ByteBuffer.wrap(hi.reverse).getInt().longValue()
    if (in < 0) in + Int.MaxValue
    else in
  }

  def readString: Array[Byte] => String = (hi: Array[Byte]) => {
    byteArr(hi).map(x => x.toChar).mkString
  }

  def readUtfString: Array[Byte] => String = (hi: Array[Byte]) => {
    byteArr(hi).map(x =>
    if (x >= 0) x.toChar
    else seq(x)).mkString
  }

  def byteArr(hi: Array[Byte]): Array[Byte] = {
    val out = new ArrayBuffer[Byte]()
    hi.foreach(x =>
    if (x != 0) out += x)
    out.toArray
  }

  def seq: Map[Int, Char] = Map(-1 -> 'я',-33 -> 'Я',
      -2 -> 'ю',-34 -> 'Ю',
      -3 -> 'э',-35 -> 'Э',
      -4 -> 'ь',-36 -> 'Ь',
      -5 -> 'ы',-37 -> 'Ы',
      -6 -> 'ъ',-38 -> 'Ъ',
      -7 -> 'щ',-39 -> 'Щ',
      -8 -> 'ш',-40 -> 'Ш',
      -9 -> 'ч',-41 -> 'Ч',
      -10 -> 'ц',-42 -> 'Ц',
      -11 -> 'х',-43 -> 'Х',
      -12 -> 'ф',-44 -> 'Ф',
      -13 -> 'у',-45 -> 'У',
      -14 -> 'т',-46 -> 'Т',
      -15 -> 'с',-47 -> 'С',
      -16 -> 'р',-48 -> 'Р',
      -17 -> 'п',-49 -> 'П',
      -18 -> 'о',-50 -> 'О',
      -19 -> 'н',-51 -> 'Н',
      -20 -> 'м',-52 -> 'М',
      -21 -> 'л',-53 -> 'Л',
      -22 -> 'к',-54 -> 'К',
      -23 -> 'й',-55 -> 'Й',
      -24 -> 'и',-56 -> 'И',
      -25 -> 'з',-57 -> 'З',
      -26 -> 'ж',-58 -> 'Ж',
      -72 -> 'ё',-88 -> 'Ё',
      -27 -> 'е',-59 -> 'Е',
      -28 -> 'д',-60 -> 'Д',
      -29 -> 'г',-61 -> 'Г',
      -30 -> 'в',-62 -> 'В',
      -31 -> 'б',-63 -> 'Б',
      -32 -> 'а',-64 -> 'А',


    )

}
