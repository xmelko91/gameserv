package utils.parsing

import java.io.ByteArrayInputStream

import akka.util.ByteString

trait InGameParse extends DataFunc {

  val DIRS: Array[Short] = Array(0, 7, 6, 5, 4, 3, 2, 1)

  def parsePocket137(data: ByteString): ParsedData137 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val nulled = readUByte(arr.readNBytes(1)(0))
    val x = readUByte(arr.readNBytes(1)(0))
    val y = readUByte(arr.readNBytes(1)(0))
    val dir = readUByte(arr.readNBytes(1)(0))
    val x1 = ((x << 2) | (y >> 6)) & 0x03FF
    val y1 = ((y << 4) | (dir >> 6)) & 0x03FF
    val dir1 = DIRS(dir & 0x0F)
    arr.close()
    ParsedData137(x1.shortValue(), y1.shortValue(), dir1)
  }

  def parsePocket159(data: ByteString): ParsedData159 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val message_count = readUShort(arr.readNBytes(2))
    val message = readUtfString(arr.readNBytes(message_count - 5))
    arr.close()
    ParsedData159(message_count, message)
  }

  def parsePocket169(data: ByteString): ParsedData169 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val ItemId = readUShort(arr.readNBytes(2))
    val EquippedId = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData169(ItemId, EquippedId)
  }

  def parsePocket171(data: ByteString): ParsedData171 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val ItemId = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData171(ItemId)
  }

  def parsePocket245(data: ByteString): ParsedData245 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    arr.readNBytes(3)
    val login = readUInteger(arr.readNBytes(4))
    val lang = readUByte(arr.readNBytes(1)(0))
    arr.readNBytes(4)
    val map = readUInteger(arr.readNBytes(4))
    arr.readNBytes(6)
    val timer = readUInteger(arr.readNBytes(4))
    val sex = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData245(login,lang,map,timer,sex)
  }

  def parsePocket278(data: ByteString): ParsedData278 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val initialSync = readUShort(arr.readNBytes(2))
    val getTimers = readUInteger(arr.readNBytes(4))
    val nulled = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData278(initialSync, getTimers, nulled)
  }

  def parsePocket1096(data: ByteString): ParsedData1096 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val local5 = readUShort(arr.readNBytes(2))
    val str1 = readUByte(arr.readNBytes(1)(0))
    val str2 = readUtfString(arr.readNBytes(32))
    arr.readNBytes(1)
    val fin = arr.readAllBytes()
    val local4 = readUtfString(fin.slice(0, fin.length - 2))
    arr.close()
    ParsedData1096(local5, str1, str2, local4)
  }




  //читает первый Шорт
  def pocketNumber(data:ByteString): Int = {
    val buf = new ByteArrayInputStream(data.toArray)
    val out = readUShort(buf.readNBytes(2))
    buf.close()
    out
  }

  case class ParsedData1096(local5: Int, str1: Short, str2: String, local4: String)
  case class ParsedData137(x: Short, y: Short, dir: Short)
  case class ParsedData159(message_count: Int, message: String)
  case class ParsedData169(ItemId: Int, EquippedId: Int)
  case class ParsedData171(ItemId: Int)
  case class ParsedData245(loginId: Long, lang: Short, mapCharId: Long, timer: Long, sex: Short)
  case class ParsedData278(initialSync: Int, getTimers: Long, nulled: Short)
}
