package utils.parsing

import java.io.ByteArrayInputStream

import akka.util.ByteString

trait InGameParse extends DataFunc {


  def parsePocket137(data: ByteString): ParsedData137 = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val nulled = readUByte(arr.readNBytes(1)(0))
    val x = readUByte(arr.readNBytes(1)(0))
    val y = readUByte(arr.readNBytes(1)(0))
    val dir = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData137(x, y, dir)
  }

  def parsePocket159(data: ByteString): ParsedData159 = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val message_count = readUShort(arr.readNBytes(2))
    val message = readUtfString(arr.readNBytes(message_count - 5))
    arr.close()
    ParsedData159(message_count, message)
  }

  def parsePocket278(data: ByteString): ParsedData278 = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val initialSync = readUShort(arr.readNBytes(2))
    val getTimers = readUInteger(arr.readNBytes(4))
    val nulled = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData278(initialSync, getTimers, nulled)
  }


  def parsePocket245(data: ByteString): ParsedData245 = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
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




  //читает первый Шорт
  def pocketNumber(data:ByteString): Int = {
    val buf = new ByteArrayInputStream(data.toArray)
    val out = readUShort(buf.readNBytes(2))
    buf.close()
    out
  }

  case class ParsedData245(loginId: Long, lang: Short, mapCharId: Long, timer: Long, sex: Short)
  case class ParsedData278(initialSync: Int, getTimers: Long, nulled: Short)
  case class ParsedData159(message_count: Int, message: String)
  case class ParsedData137(x: Short, y: Short, dir: Short)
}
