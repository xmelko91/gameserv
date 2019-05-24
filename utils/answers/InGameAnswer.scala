package utils.answers

import akka.http.scaladsl.model.DateTime
import akka.util.ByteString
import utils.parsing.MathUtils

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


  def pocket141Answer(CharacterId: Long, isGm: Int, baseLevel: Int, PremiumType: Short, Race: Short, message: String, msgLength: Int): Answer = {
    val arr = shortToByteArray(141) ++
      shortToByteArray(msgLength + 12) ++
      intToByteArray(CharacterId) ++
      shortToByteArray((isGm * 256) + baseLevel) ++
      byteToByteArray(PremiumType) ++
      byteToByteArray(Race) ++
      stringToByteArray(message, msgLength)
    Answer(ByteString(arr))
  }


  def pocket142Answer(message: String): Answer = {
    val arr = shortToByteArray(142) ++
      intToByteArray(message.length + 4) ++
      stringToByteArray(message, 0)
    Answer(ByteString(arr))
  }

  def pocket170Answer(ItemId: Short, EquippedId: Short): Answer = {
    val arr = shortToByteArray(170) ++
      shortToByteArray(ItemId) ++
      shortToByteArray(EquippedId) ++
      byteToByteArray(0)
    Answer(ByteString(arr))
  }

  def pocket172Answer(ItemId: Short): Answer = {
    val arr = shortToByteArray(172) ++
      shortToByteArray(ItemId) ++
      shortToByteArray(0) ++
      byteToByteArray(0)
    Answer(ByteString(arr))
  }

  def pocket176Answer(stats_switch: Short, stats: Int): Answer = {
    val arr = shortToByteArray(176) ++
      shortToByteArray(stats_switch) ++
      intToByteArray(stats)
    Answer(ByteString(arr))
  }

  def pocket189Answer(Stats_count: Int, _str: Short, _str_append: Short,
                      _agi: Short, _agi_append: Short, _vit: Short,
                      _vit_append: Short, _int: Short, _int_append: Short,
                      _dex: Short, _dex_append: Short, _luk: Short,
                      _luk_append: Short, atk1: Int, atk2: Int,
                      matkMax: Int, matkMin: Int, def1: Int,
                      def2: Int, mdef1: Int, mdef2: Int,
                      hit: Int, flee1: Int, flee2: Int,
                      critical: Int, karma: Int, manner: Int): Answer =
  {
    val arr = shortToByteArray(189) ++
      shortToByteArray(Stats_count) ++
      byteToByteArray(_str) ++
      byteToByteArray(_str_append) ++
      byteToByteArray(_agi) ++
      byteToByteArray(_agi_append) ++
      byteToByteArray(_vit) ++
      byteToByteArray(_vit_append) ++
      byteToByteArray(_int) ++
      byteToByteArray(_int_append) ++
      byteToByteArray(_dex) ++
      byteToByteArray(_dex_append) ++
      byteToByteArray(_luk) ++
      byteToByteArray(_luk_append) ++
      shortToByteArray(atk1) ++
      shortToByteArray(atk2) ++
      shortToByteArray(matkMax) ++
      shortToByteArray(matkMin) ++
      shortToByteArray(def1) ++
      shortToByteArray(def2) ++
      shortToByteArray(mdef1) ++
      shortToByteArray(mdef2) ++
      shortToByteArray(hit) ++
      shortToByteArray(flee1) ++
      shortToByteArray(flee2) ++
      shortToByteArray(critical) ++
      shortToByteArray(karma) ++
      shortToByteArray(manner)
    Answer(ByteString(arr))
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

  def pocket451Answer(message: String, l1: Short=0, l2: Short=0, l3:Short=0, l6: Int=0): Answer = {
    val arr = shortToByteArray(451) ++
      shortToByteArray(message.length + 16) ++
      byteToByteArray(l1) ++
      byteToByteArray(l2) ++
      byteToByteArray(l3) ++
      byteToByteArray(0) ++
      shortToByteArray(l6) ++
      stringToByteArray("",6) ++
      stringToByteArray(message,message.length)
    Answer(ByteString(arr))
  }

  def pocket556Answer(CharacteId: Long, walk_speed: Int, options1: Int,
                      options2: Int, options3: Int, jobid: Int,
                      tochka: Int, viewWeapon: Int, viewShield: Int,
                      viewHead: Int, hairColor: Int, clothesColor: Int,
                      guildEmblem: Int, manner: Int, _local_11: Long,
                      karma: Short, sex: Short, x: Short, y: Short, x1: Short,
                      y1: Short, sx: Short, sy: Short, baseLevel: Int): Answer = {


    val arr = shortToByteArray(556) ++
      stringToByteArray("", 1) ++
      intToByteArray(CharacteId) ++
      shortToByteArray(walk_speed) ++
      shortToByteArray(options1) ++
      shortToByteArray(options2) ++
      intToByteArray(options3) ++
      shortToByteArray(jobid) ++
      shortToByteArray(tochka) ++
      shortToByteArray(viewWeapon) ++
      shortToByteArray(viewShield) ++
      stringToByteArray("", 6) ++
      shortToByteArray(viewHead) ++
      stringToByteArray("", 2) ++
      shortToByteArray(hairColor) ++
      shortToByteArray(clothesColor) ++
      stringToByteArray("", 6) ++
      shortToByteArray(guildEmblem) ++
      shortToByteArray(manner) ++
      intToByteArray(_local_11) ++
      byteToByteArray(karma) ++
      byteToByteArray(sex) ++
      SetEncodedArray6(x,y,x1,y1,sy,sx) ++
      stringToByteArray("", 2) ++
      shortToByteArray(baseLevel)
    Answer(ByteString(arr))
  }

  def pocket643Answer(CharacterId: Long): Answer = {
    val arr = shortToByteArray(643) ++
      intToByteArray(CharacterId)
    Answer(ByteString(arr))
  }

  def pocket1025Answer(count_Quests: Short, id_Quests: Short, progress_Quests: Byte): Answer = {
    val arr = shortToByteArray(1025) ++
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

  def pocketGold(gold: Long): Answer = {
    val arr = shortToByteArray(1024) ++
      intToByteArray(gold)
    Answer(ByteString(arr))
  }


  case class Answer(data: ByteString)

}
