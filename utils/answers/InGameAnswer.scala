package utils.answers

import akka.http.scaladsl.model.DateTime
import akka.util.ByteString
import app.actors.preStartGame.LoginActor.LoginAnswerOut
import utils.parsing.{MathUtils}

trait InGameAnswer extends MathUtils {

  def pocket115Answer(CharacterId: Long, x: Short, y: Short, dir: Short): Answer = {
    val arr = shortToByteArray(115) ++
      intToByteArray(CharacterId) ++
      byteToByteArray(x) ++
      byteToByteArray(y) ++
      byteToByteArray(dir) ++
      shortToByteArray(0)
    Answer(ByteString(arr))
  }

  def pocket127Answer(utils: Int = 0): Answer = {
    val arr = shortToByteArray(127) ++
      intToByteArray(utils)
    Answer(ByteString(arr))
  }

  def pocket128Answer(CharacterId: Int, switch_count: Byte = 0): Answer = {
    val arr = shortToByteArray(128) ++
      intToByteArray(CharacterId) ++
      byteToByteArray(switch_count)
    Answer(ByteString(arr))
  }

  def pocket136Answer(CharacterId: Int, x: Short, y: Short): Answer = {
    val arr = shortToByteArray(136) ++
      intToByteArray(CharacterId) ++
      shortToByteArray(x) ++
      shortToByteArray(y)
    Answer(ByteString(arr))
  }

  /*def pocket189Answer(Stats_count: Short, _str: Byte, _str_append: Byte, _agi: Byte, _agi_append: Byte, _vit: Byte, _vit_append: Byte, _int: Byte, _int_append: Byte, _dex: Byte, _dex_append: Byte, _luk: Byte, _luk_append: Byte, x1: Short, x2: Short, x3: Short, x4: Short, x5: Short, x6: Short, x7: Short, x8: Short, x9: Short, x10: Short, x11: Short, x12: Short, x13: Short, x14: Short): LoginAnswerOut =
  {
    val arr = shortToByteArray(189) ++ intToByteArray()
    LoginAnswerOut(ByteString(arr))
  }*/


  def pocket141Answer(message_count: Short, CharacterId: Int, isGm: Short, baseLevel: Short, PremiumType: Byte, Race: Byte, message: String): Answer = {
    val arr = shortToByteArray(141) ++
      shortToByteArray(message_count + 12) ++
      intToByteArray(CharacterId) ++
      shortToByteArray((isGm * 256) + baseLevel)
    byteToByteArray(PremiumType) ++
      byteToByteArray(Race) ++
      stringToByteArray(message, 0)
    Answer(ByteString(arr))
  }


  def pocket142Answer(message: String): Answer = {
    val arr = shortToByteArray(142) ++
      intToByteArray(message.length + 4) ++
      stringToByteArray(message, 0)
    Answer(ByteString(arr))
  }

  def pocket176Answer(stats_switch: Short, stats: Int): Answer = {
    val arr = shortToByteArray(176) ++
      shortToByteArray(stats_switch) ++
      intToByteArray(stats)
    Answer(ByteString(arr))
  }

  def pocket189Answer(): Answer = {
    Answer(ByteString(0))
  }

  def pocket321Answer(stats_switch: Short, stats: Int, stats_bonus: Int): Answer = {
    val arr = shortToByteArray(321) ++
      shortToByteArray(stats_switch) ++
      intToByteArray(stats) ++
      intToByteArray(stats_bonus)
    Answer(ByteString(arr))
  }

  def pocket411Answer(CharacterId: Long, switch_count: Int): Answer = {
    val arr = shortToByteArray(411) ++
      intToByteArray(CharacterId) ++
      intToByteArray(switch_count)
    Answer(ByteString(arr))
  }

  def pocket643Answer(CharacterId: Long): Answer = {
    val arr = shortToByteArray(643) ++
      intToByteArray(CharacterId)
    Answer(ByteString(arr))
  }

  def pocket1024Answer(count_Quests: Short, id_Quests: Short, progress_Quests: Byte): Answer = {
    val arr = shortToByteArray(1024) ++
      shortToByteArray(count_Quests) ++
      shortToByteArray(id_Quests) ++
      byteToByteArray(progress_Quests)
    Answer(ByteString(arr))
  }

  def pocket1059Answer(date: DateTime, utils: Short = 32112): Answer = {
    val arr = shortToByteArray(1059) ++
      dateToArray(date) ++
      shortToByteArray(utils)
    Answer(ByteString(arr))
  }

  def pocketGold(gold : Long): Answer = {
    val arr = intToByteArray(0x0400) ++
      intToByteArray(gold)
    Answer(ByteString(arr))
  }



  case class Answer(data: ByteString)

}
