package utils.answers

import akka.util.ByteString
import app.actors.preStartGame.LoginActor.LoginAnswerOut
import utils.parsing.DataFunc

trait InGameAnswer extends DataFunc {

  def pocket115Answer(CharacterId: Long, x: Short, y: Short, dir: Short): LoginAnswerOut = {
    val arr = shortToByteArray(115) ++
      intToByteArray(CharacterId) ++
      byteToByteArray(x) ++
      byteToByteArray(y) ++
      byteToByteArray(dir) ++
      shortToByteArray(0)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket127Answer(utils: Int = 0): LoginAnswerOut = {
    val arr = shortToByteArray(127) ++
      intToByteArray(utils)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket128Answer(CharacterId: Int, switch_count: Byte = 0): LoginAnswerOut = {
    val arr = shortToByteArray(128) ++
      intToByteArray(CharacterId) ++
      byteToByteArray(switch_count)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket136Answer(CharacterId: Int, x: Short, y: Short): LoginAnswerOut = {
    val arr = shortToByteArray(136) ++
      intToByteArray(CharacterId) ++
      shortToByteArray(x) ++
      shortToByteArray(y)
    LoginAnswerOut(ByteString(arr))
  }

  /*def pocket189Answer(Stats_count: Short, _str: Byte, _str_append: Byte, _agi: Byte, _agi_append: Byte, _vit: Byte, _vit_append: Byte, _int: Byte, _int_append: Byte, _dex: Byte, _dex_append: Byte, _luk: Byte, _luk_append: Byte, x1: Short, x2: Short, x3: Short, x4: Short, x5: Short, x6: Short, x7: Short, x8: Short, x9: Short, x10: Short, x11: Short, x12: Short, x13: Short, x14: Short): LoginAnswerOut =
  {
    val arr = shortToByteArray(189) ++ intToByteArray()
    LoginAnswerOut(ByteString(arr))
  }*/


  def pocket141Answer(message_count: Short, CharacterId: Int, isGm: Short, baseLevel: Short, PremiumType: Byte, Race: Byte, message: String): LoginAnswerOut = {
    val arr = shortToByteArray(141) ++
      shortToByteArray(message_count + 12) ++
      intToByteArray(CharacterId) ++
      shortToByteArray((isGm * 256) + baseLevel)
    byteToByteArray(PremiumType) ++
      byteToByteArray(Race) ++
      stringToByteArray(message, 0)
    LoginAnswerOut(ByteString(arr))
  }


  def pocket142Answer(message: String): LoginAnswerOut = {
    val arr = shortToByteArray(142) ++
      intToByteArray(message.length + 4) ++
      stringToByteArray(message, 0)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket176Answer(stats_switch: Short, stats: Int): LoginAnswerOut = {
    val arr = shortToByteArray(176) ++
      shortToByteArray(stats_switch) ++
      intToByteArray(stats)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket321Answer(stats_switch: Short, stats: Int, stats_bonus: Int): LoginAnswerOut = {
    val arr = shortToByteArray(321) ++
      shortToByteArray(stats_switch) ++
      intToByteArray(stats) ++
      intToByteArray(stats_bonus)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket411Answer(CharacterId: Long, switch_count: Int): LoginAnswerOut = {
    val arr = shortToByteArray(411) ++
      intToByteArray(CharacterId) ++
      intToByteArray(switch_count)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket643Answer(CharacterId: Long): LoginAnswerOut = {
    val arr = shortToByteArray(643) ++
      intToByteArray(CharacterId)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket1024Answer(count_Quests: Short, id_Quests: Short, progress_Quests: Byte): LoginAnswerOut = {
    val arr = shortToByteArray(1024) ++
      shortToByteArray(count_Quests) ++
      shortToByteArray(id_Quests) ++
      byteToByteArray(progress_Quests)
    LoginAnswerOut(ByteString(arr))
  }

  def pocket1059Answer(time: Int, utils: Short = 0): LoginAnswerOut = {
    val arr = shortToByteArray(1059) ++
      intToByteArray(time) ++
      shortToByteArray(utils)
    LoginAnswerOut(ByteString(arr))
  }


}
