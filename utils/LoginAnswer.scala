package utils
import java.nio._

import akka.util.ByteString
import app.LoginActor.LoginAnswerOut

import scala.collection.mutable.ArrayBuffer

object LoginAnswer {

  import DataFunc._

  def composeArray(NumberOfPocket:Short,
                   Size:Short,
                   LoginId1: Int,
                   LoginAccountId: Int,
                   LoginId2: Int,
                   NumSlots: Byte,
                   PremiumType:Byte,
                   PremiumUntil: Int,
                   HbmVersion: Int,
                   SexOld: Byte,
                   ExtraEmptyBytes: Int):LoginAnswerOut = {
    val arr = shortToByteArray(NumberOfPocket) ++
      shortToByteArray(Size) ++
      intToByteArray(LoginId1) ++
      intToByteArray(LoginAccountId) ++
      intToByteArray(LoginId2) ++
      byteToByteArray(NumSlots) ++
      byteToByteArray(PremiumType) ++
      intToByteArray(PremiumUntil) ++
      intToByteArray(HbmVersion) ++
      new Array[Byte](ExtraEmptyBytes) ++
      byteToByteArray(SexOld)
    println(arr.length)
    LoginAnswerOut(ByteString(arr))
  }

}
