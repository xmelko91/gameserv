package utils.parsing

import java.io.ByteArrayInputStream

import akka.util.ByteString

object ParserServ {

  import DataFunc._


  def parsePocket100(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val l1 = readShort(arr.readNBytes(2))
    val l2 = readShort(arr.readNBytes(2))
    val EMAIL = readString(arr.readNBytes(24))
    val PASSWORD = readString(arr.readNBytes(25))
    val BYTES = arr.readNBytes(1)(0)
    arr.close()
    ParsedData100(l1, l2, EMAIL, PASSWORD, BYTES)
  }


  def parsePocket102(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val SLOTID = arr.readNBytes(1)(0)
    arr.close()
    ParsedData102(SLOTID)
  }

  def parsePocket103(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val NICKNAME = readString(arr.readNBytes(24))
    val STR = arr.readNBytes(1)(0)
    val AGI = arr.readNBytes(1)(0)
    val VIT = arr.readNBytes(1)(0)
    val INT = arr.readNBytes(1)(0)
    val DEX = arr.readNBytes(1)(0)
    val LUK = arr.readNBytes(1)(0)
    val SLOTID = arr.readNBytes(1)(0)
    val JOBID = readShort(arr.readNBytes(2))
    val ISMALE = readShort(arr.readNBytes(2))
    arr.close()
    ParsedData103(NICKNAME, STR, AGI, VIT, INT, DEX, LUK, SLOTID, JOBID, ISMALE)
  }


  //читает первый Шорт
  def pocketNumber(data:ByteString) = {
    val buf = new ByteArrayInputStream(data.toArray)
    val out = readShort(buf.readNBytes(2))
    buf.close()
    out
  }

  case class ParsedData100(_packet:Short, _short:Short, email:String, password:String, _bytes:Byte)
  case class ParsedData102(_slotid:Byte)
  case class ParsedData103(_nickname:String, _str:Byte, _agi:Byte, _vit:Byte, _int:Byte, _dex:Byte, _luk:Byte, _slotid:Byte, _jobid:Short, _ismale:Short)
}
