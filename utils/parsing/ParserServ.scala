package utils.parsing

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import akka.util.ByteString
import app.PocketLogic.CheckBase

trait ParserServ extends DataFunc with CheckBase{


  def parsePocket100(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val l2 = readUShort(arr.readNBytes(2))
    val EMAIL = readString(arr.readNBytes(26))
    val ar = arr.readNBytes(25)
    val PASSWORD = readString(ar)
    val BYTES = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData100(pocketNumb, l2, EMAIL, PASSWORD, BYTES)
  }

  def parsePocket101(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val loginAccountId = readUInteger(arr.readNBytes(4))
    val loginId1 = readUInteger(arr.readNBytes(4))
    val loginId2 = readUInteger(arr.readNBytes(4))
    readUShort(arr.readNBytes(2))
    val sexOld = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData101(loginAccountId,loginId1,loginId2,0,sexOld)
  }

  def parsePocket102(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val SLOTID = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData102(SLOTID)
  }

  def parsePocket103(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val NICKNAME = readUtfString(arr.readNBytes(24))
    val STR = readUByte(arr.readNBytes(1)(0))
    val AGI = readUByte(arr.readNBytes(1)(0))
    val VIT = readUByte(arr.readNBytes(1)(0))
    val INT = readUByte(arr.readNBytes(1)(0))
    val DEX = readUByte(arr.readNBytes(1)(0))
    val LUK = readUByte(arr.readNBytes(1)(0))
    val SLOTID = readUByte(arr.readNBytes(1)(0))
    val JOBID = readUShort(arr.readNBytes(2))
    val ISMALE = readUShort(arr.readNBytes(2))
    val summ = (STR + AGI + VIT + INT + DEX + LUK).byteValue()
    arr.close()
    ParsedData103(NICKNAME, STR, AGI, VIT, INT, DEX, LUK, SLOTID, JOBID, ISMALE, summ)
  }

  def parsePocket653(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val LoginAccountId = readUInteger(arr.readNBytes(4))
    val CharacterID = readUInteger(arr.readNBytes(4))
    val NICKNAME = readUtfString(arr.readNBytes(24))
    arr.close()
    ParsedData653(LoginAccountId, CharacterID, NICKNAME)
  }

  def parsePocket655(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val pocketNumb = readUShort(arr.readNBytes(2))
    val CharacterID = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData655(CharacterID)
  }



  //читает первый Шорт
  def pocketNumber(data:ByteString) = {
    val buf = new ByteArrayInputStream(data.toArray)
    val out = readUShort(buf.readNBytes(2))
    buf.close()
    out
  }

  case class ParsedData100(_packet:Int, _short:Int, email:String, password:String, _bytes:Short)
  case class ParsedData101(loginAccountId: Long, loginId1: Long, loginId2: Long, short: Int, sexOld: Short)
  case class ParsedData102(_slotid:Short)
  case class ParsedData103(_nickname:String, _str:Short, _agi:Short, _vit:Short, _int:Short, _dex:Short, _luk:Short, _slotid:Short, _jobid:Int, _ismale:Int, summ: Short)
  case class ParsedData653(LoginAccountId: Long, ChacharacterID: Long, NICKNAME: String)
  case class ParsedData655(ChacharacterID: Long)
}
