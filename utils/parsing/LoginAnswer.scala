package utils.parsing


import akka.util.ByteString
import app.actors.LoginActor.LoginAnswerOut
import utils.sqlutils.SQLActor.PlayerLoginStats

import scala.io.Source
import scala.util.control.Breaks

object LoginAnswer {


  import app.Settings._
  import DataFunc._


  var adress1:Byte = TCP_IP._1
  var adress2:Byte = TCP_IP._2
  var adress3:Byte = TCP_IP._3
  var adress4:Byte = TCP_IP._4
  var port: Short = TCP_IP._5


  val NumberOfPocket:Short = 105
  val ExtraEmptyBytes: Int = 20
  val SexOld: Byte = 0


  def pocket105Answer(Size:Short, Stats: PlayerLoginStats,
                      HbmVersion: Int,
                      name: String,
                      nameBuf: Int,
                      users: Short,
                      maintenance: Short,
                      New: Short
                  ):LoginAnswerOut = {


    val arr = shortToByteArray(NumberOfPocket) ++
      shortToByteArray(Size) ++
      intToByteArray(Stats.loginId1) ++
      intToByteArray(Stats.loginAccId) ++
      intToByteArray(Stats.loginId2) ++
      byteToByteArray(Stats.numSlots) ++
      byteToByteArray(Stats.premiumType) ++
      intToByteArray(Stats.premiumUntil) ++
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
    val arr = ???
      LoginAnswerOut(arr)
  }

}
