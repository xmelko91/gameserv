package utils.answers

import akka.util.ByteString
import app.actors.LoginActor.LoginAnswerOut
import utils.parsing.DataFunc
import utils.sqlutils.SQLActor.PlayerLoginStats

object LoginAnswer {


  import DataFunc._
  import app.Settings._


  var adress1:Byte = TCP_IP._1
  var adress2:Byte = TCP_IP._2
  var adress3:Byte = TCP_IP._3
  var adress4:Byte = TCP_IP._4
  var port: Short = TCP_IP._5


  def pocket105Answer(Size:Short, Stats: PlayerLoginStats,
                      HbmVersion: Int,
                      name: String,
                      nameBuf: Int,
                      users: Short,
                      maintenance: Short,
                      New: Short
                  ):LoginAnswerOut = {


    val arr = shortToByteArray(105) ++
      shortToByteArray(Size) ++
      intToByteArray(Stats.loginId1) ++
      intToByteArray(Stats.loginAccId) ++
      intToByteArray(Stats.loginId2) ++
      byteToByteArray(Stats.numSlots) ++
      byteToByteArray(Stats.premiumType) ++
      intToByteArray(Stats.premiumUntil) ++
      intToByteArray(HbmVersion) ++
      new Array[Byte](20) ++
      byteToByteArray(0) ++
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

  def pocket106Answer(Errors:Byte,
                      Message_Errors: String
                     ): LoginAnswerOut = {
    val arr = shortToByteArray(106) ++
      byteToByteArray(Errors) ++
      stringToByteArray(Message_Errors, 20)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket110Answer(Errors:Byte): LoginAnswerOut = {
    val arr = shortToByteArray(110) ++
      byteToByteArray(Errors)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket113Answer(CharacterId: Int,
                      MapName: String): LoginAnswerOut = {
    val arr = shortToByteArray(113) ++
      intToByteArray(CharacterId) ++
      stringToByteArray(MapName, 16) ++
      byteToByteArray(127) ++
      byteToByteArray(0) ++
      byteToByteArray(0) ++
      byteToByteArray(1) ++
      shortToByteArray(2970)
    LoginAnswerOut(ByteString(arr))
  }

}
