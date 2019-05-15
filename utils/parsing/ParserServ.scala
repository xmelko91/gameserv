package utils.parsing

import java.io.ByteArrayInputStream

import akka.util.ByteString

object ParserServ {

  import DataFunc._


  def parseLogIn(data: ByteString) = {
    val arr = new ByteArrayInputStream(data.toArray)
    val l1 = readShort(arr.readNBytes(2))
    val l2 = readInteger(arr.readNBytes(4))
    val EMAIL = readString(arr.readNBytes(24))
    val PASSWORD = readString(arr.readNBytes(25))
    println(EMAIL + " " + PASSWORD)
    ParsedData(l1, l2, EMAIL, PASSWORD)
  }

  //читает первый Шорт
  def pocketNumber(data:ByteString) = {
    val buf = new ByteArrayInputStream(data.toArray)
    val out = readShort(buf.readNBytes(2))
    buf.close()
    out
  }

  case class ParsedData(short:Short, int: Int, email:String, password:String)
}
