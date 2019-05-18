package utils.answers

import akka.util.ByteString
import app.actors.LoginActor.LoginAnswerOut
import utils.answers.CharacterAnswer.CharStats
import utils.parsing.DataFunc
import utils.sqlutils.SQLActor.PlayerLoginStats

import scala.collection.mutable.ArrayBuffer

trait LoginAnswer extends DataFunc {


  import app.Settings._


  var adress1: Byte = TCP_IP._1
  var adress2: Byte = TCP_IP._2
  var adress3: Byte = TCP_IP._3
  var adress4: Byte = TCP_IP._4
  var port: Short = TCP_IP._5


  def pocket105Answer(Size: Int, Stats: PlayerLoginStats,
                      HbmVersion: Long,
                      name: String,
                      nameBuf: Long,
                      users: Int,
                      maintenance: Int,
                      New: Int
                     ): LoginAnswerOut = {


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

  def pocket106Answer(Errors: Short,
                      Message_Errors: String
                     ): LoginAnswerOut = {
    val arr = shortToByteArray(106) ++
      byteToByteArray(Errors) ++
      stringToByteArray(Message_Errors, 20)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket107Answer(arrayChars: Array[CharStats], slot: Int): LoginAnswerOut = {

    var arr = shortToByteArray(107) ++
      shortToByteArray(20) ++
      stringToByteArray("", 20)

    if (slot == -1) {
      for (p <- arrayChars) {
        arr = arr ++ (intToByteArray(p.charId) ++
          intToByteArray(p.baseExp) ++
          intToByteArray(p.money) ++
          intToByteArray(p.jobExp) ++
          shortToByteArray(p.jobLvl) ++
          stringToByteArray("", 24) ++
          shortToByteArray(p.hp) ++
          shortToByteArray(p.maxHp) ++
          shortToByteArray(p.sp) ++
          shortToByteArray(p.maxSp) ++
          stringToByteArray("", 2) ++
          shortToByteArray(p.jobId) ++
          shortToByteArray(p.local3Sex) ++
          stringToByteArray("", 2) ++
          shortToByteArray(p.baseLvl) ++
          stringToByteArray("", 2) ++
          shortToByteArray(0) ++
          stringToByteArray("", 2) ++
          shortToByteArray(0) ++
          shortToByteArray(0) ++
          shortToByteArray(p.hairColor) ++
          shortToByteArray(p.clothesColor) ++
          stringToByteArray(p.name, 24) ++
          byteToByteArray(p.str) ++
          byteToByteArray(p.agi) ++
          byteToByteArray(p.vit) ++
          byteToByteArray(p.int) ++
          byteToByteArray(p.dex) ++
          byteToByteArray(p.luk) ++
          shortToByteArray(p.slot) ++
          shortToByteArray(p.renames))
        println(p.name)
        arr.zipWithIndex.foreach(z => println(z._2 + " index is " + z._1 ))
        println(p.local3Sex)
      }
    }else{
      for (p <- arrayChars) {
        if (p.slot == slot) {
          arr = arr ++ (intToByteArray(p.charId) ++
            intToByteArray(p.baseExp) ++
            intToByteArray(p.money) ++
            intToByteArray(p.jobExp) ++
            shortToByteArray(p.jobLvl) ++
            stringToByteArray("", 24) ++
            shortToByteArray(p.hp) ++
            shortToByteArray(p.maxHp) ++
            shortToByteArray(p.sp) ++
            shortToByteArray(p.maxSp) ++
            stringToByteArray("", 2) ++
            shortToByteArray(p.jobId) ++
            shortToByteArray(p.local3Sex) ++
            stringToByteArray("", 2) ++
            shortToByteArray(p.baseLvl) ++
            stringToByteArray("", 2) ++
            shortToByteArray(0) ++
            stringToByteArray("", 2) ++
            shortToByteArray(0) ++
            shortToByteArray(0) ++
            shortToByteArray(p.hairColor) ++
            shortToByteArray(p.clothesColor) ++
            stringToByteArray(p.name, 24) ++
            byteToByteArray(p.str) ++
            byteToByteArray(p.agi) ++
            byteToByteArray(p.vit) ++
            byteToByteArray(p.int) ++
            byteToByteArray(p.dex) ++
            byteToByteArray(p.luk) ++
            shortToByteArray(p.slot) ++
            shortToByteArray(p.renames))
        }
      }
    }

    LoginAnswerOut(ByteString(arr))
  }

  def pocket109NewChar(characterId: Long,
                       jobId: Int,
                       local3: Int,
                       hairColor: Int,
                       clothesColor: Int,
                       NickName: String): LoginAnswerOut =
    pocket109Answer(characterId, 5, 50000, 5, 1, 42, 42, 10, 13, jobId, local3, 1, hairColor, clothesColor, NickName, 5, 5, 5, 5, 5, 5, 0, 1)

  def pocket109Answer(CharacterId: Long,
                      BaseExp: Long,
                      Money: Long,
                      JobExp: Long,
                      JobLevel: Int,
                      hp: Int,
                      maxHp: Int,
                      sp: Int,
                      maxSp: Int,
                      JobId: Int,
                      _local_3: Int,
                      baseLevel: Int,
                      hairColor: Int,
                      clothesColor: Int,
                      NickName: String,
                      _str: Short,
                      _agi: Short,
                      _vit: Short,
                      _int: Short,
                      _dex: Short,
                      _luk: Short,
                      _slot: Int,
                      _renames: Int): LoginAnswerOut = {
    val arr = shortToByteArray(109) ++
      intToByteArray(CharacterId) ++
      intToByteArray(BaseExp) ++
      intToByteArray(Money) ++
      intToByteArray(JobExp) ++
      shortToByteArray(JobLevel) ++
      stringToByteArray("", 24) ++
      shortToByteArray(hp) ++
      shortToByteArray(maxHp) ++
      shortToByteArray(sp) ++
      shortToByteArray(maxSp) ++
      stringToByteArray("", 2) ++
      shortToByteArray(JobId) ++
      shortToByteArray(_local_3) ++
      stringToByteArray("", 2) ++
      shortToByteArray(baseLevel) ++
      stringToByteArray("", 2) ++
      intToByteArray(0) ++
      stringToByteArray("", 2) ++
      intToByteArray(0) ++
      intToByteArray(0) ++
      shortToByteArray(hairColor) ++
      shortToByteArray(clothesColor) ++
      stringToByteArray(NickName, 24) ++
      byteToByteArray(_str) ++
      byteToByteArray(_agi) ++
      byteToByteArray(_vit) ++
      byteToByteArray(_int) ++
      byteToByteArray(_dex) ++
      byteToByteArray(_luk) ++
      shortToByteArray(_slot) ++
      shortToByteArray(_renames)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket110Answer(Errors: Short): LoginAnswerOut = {
    val arr = shortToByteArray(110) ++
      byteToByteArray(Errors)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket113Answer(CharacterId: Long,
                      MapName: String): LoginAnswerOut = {
    val arr = shortToByteArray(113) ++
      intToByteArray(CharacterId) ++
      stringToByteArray(MapName, 16) ++
      byteToByteArray(TCP_IP._1) ++
      byteToByteArray(TCP_IP._2) ++
      byteToByteArray(TCP_IP._3) ++
      byteToByteArray(TCP_IP._4) ++
      shortToByteArray(TCP_IP._5)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket115Answer(int: Long, short: Int): LoginAnswerOut = {
    val arr = shortToByteArray(115) ++
      intToByteArray(int) ++
      shortToByteArray(short)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket654Answer(errors: Int): LoginAnswerOut = {
    val arr = shortToByteArray(654) ++
      shortToByteArray(errors)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket656Answer(errors: Int): LoginAnswerOut = {
    val arr = shortToByteArray(656) ++
      shortToByteArray(errors)
    LoginAnswerOut(ByteString(arr))
  }

}
