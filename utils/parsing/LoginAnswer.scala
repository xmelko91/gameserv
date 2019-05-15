package utils.parsing

import akka.util.ByteString
import app.actors.LoginActor.LoginAnswerOut

object LoginAnswer {

  import DataFunc._

  def pocket100Answer(NumberOfPocket:Short,
                      Size:Short,
                      LoginId1: Int,
                      LoginAccountId: Int,
                      LoginId2: Int,
                      NumSlots: Byte,
                      PremiumType:Byte,
                      PremiumUntil: Int,
                      HbmVersion: Int,
                      ExtraEmptyBytes: Int,
                      SexOld: Byte,
                      adress1:Byte,
                      adress2:Byte,
                      adress3:Byte,
                      adress4:Byte,
                      port: Short,
                      name: String,
                      nameBuf: Int,
                      users: Short,
                      maintenance: Short,
                      New: Short
                  ):LoginAnswerOut = {
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
      byteToByteArray(SexOld) ++
      byteToByteArray(adress1) ++
      byteToByteArray(adress2) ++
      byteToByteArray(adress3) ++
      byteToByteArray(adress4) ++
      shortToByteArray(port) ++
      stringToByteArray(name, nameBuf) ++
      shortToByteArray(users) ++
      shortToByteArray(maintenance) ++
      shortToByteArray(New)
    println(arr.length)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket101Answer(): LoginAnswerOut = {
    val arr =

      LoginAnswerOut(arr)
  }

}
