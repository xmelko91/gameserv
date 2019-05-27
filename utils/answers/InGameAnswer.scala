package utils.answers

import akka.http.scaladsl.model.DateTime
import akka.util.ByteString
import app.actors.inGame.InGamePlayer.CalculatedStats
import utils.parsing.MathUtils
import utils.sqlutils.MapSQL.ItemsSet

import scala.collection.mutable.ArrayBuffer

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

  def pocket128Answer(CharacterId: Long, switch_count: Byte = 0): Answer = {
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

  def pocket164Answer(items: Array[ItemsSet]): Answer = {
    var buf = Array[Byte]()
    val It = items.filter(x => x.iemParams.tyype > 0)
    It.foreach(I => {
      buf  = buf ++ shortToByteArray(I.item.id.intValue()) ++
        shortToByteArray(I.item.nameId) ++
        byteToByteArray(I.item.tyype) ++
        byteToByteArray(I.item.identified) ++
        shortToByteArray(I.item.typeEquip) ++
        shortToByteArray(I.item.equip) ++
        byteToByteArray(I.item.attr) ++
        byteToByteArray(I.item.upgrade)
      /*for (x <- 0 to I.iemParams.slots){
        if (x < 4){
          buf = buf ++ shortToByteArray(x match {
            case 0 => I.item.slot1.intValue()
            case 1 => I.item.slot2.intValue()
            case 2 => I.item.slot3.intValue()
            case 3 => I.item.slot4.intValue()
          })
        }else{
          buf  = buf ++ intToByteArray(x match {
            case 4 => I.item.others1
            case 5 => I.item.others2
            case 6 => I.item.others3
            case 7 => I.item.others4
            case 8 => I.item.others5
            case 9 => I.item.others6
          })
        }
      }*/
    })
    println(It.length)
    val arr = shortToByteArray(164) ++
      shortToByteArray(It.length * 44 + 4) ++
      buf
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

  def pocket179Answer(): Answer = {
    val arr = shortToByteArray(179) ++
      byteToByteArray(1)
    Answer(ByteString(arr))
  }

  def pocket188Answer(operEnd:Short, statN: Int, stat:Short): Answer = {
    val arr = shortToByteArray(188) ++
      shortToByteArray(statN) ++
      byteToByteArray(operEnd) ++
      byteToByteArray(stat)
    Answer(ByteString(arr))
  }

  def pocket189Answer(s: CalculatedStats): Answer = pocket189Answer(s.Stats_count, s._str, s._str_append, s._agi,
    s._agi_append, s._vit, s._vit_append, s._int, s._int_append, s._dex, s._dex_append,
    s._luk, s._luk_append, s.atk1, s.atk2, s.matkMax, s.matkMin, s.def1, s.def2,
    s.mdef1, s.mdef2, s.hit, s.flee1, s.flee2, s.critical, s.karma, s.manner)

  def pocket189Answer(Stats_count: Int, _str: Short, _str_append: Short,
                      _agi: Short, _agi_append: Short, _vit: Short,
                      _vit_append: Short, _int: Short, _int_append: Short,
                      _dex: Short, _dex_append: Short, _luk: Short,
                      _luk_append: Short, atk1: Int, atk2: Int,
                      matkMax: Int, matkMin: Int, def1: Int,
                      def2: Int, mdef1: Int, mdef2: Int,
                      hit: Int, flee1: Int, flee2: Int,
                      critical: Int, karma: Int, manner: Int): Answer = {
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

  def pocket451Answer(message: String, l1: Short = 0, l2: Short = 0, l3: Short = 0, l6: Int = 0): Answer = {
    val arr = shortToByteArray(451) ++
      shortToByteArray(message.length + 16) ++
      byteToByteArray(l1) ++
      byteToByteArray(l2) ++
      byteToByteArray(l3) ++
      byteToByteArray(0) ++
      shortToByteArray(l6) ++
      stringToByteArray("", 6) ++
      stringToByteArray(message, message.length)
    Answer(ByteString(arr))
  }

  def pocket494Answer(items: Array[ItemsSet]): Answer = {
    var buf = Array[Byte]()
    val It = items.filter(x => x.iemParams.tyype <= 0)
    It.foreach(I => {
      buf  = buf ++ shortToByteArray(I.item.id.intValue()) ++
        shortToByteArray(I.item.nameId) ++
        byteToByteArray(I.item.tyype) ++
        byteToByteArray(0) ++
        shortToByteArray(I.item.amount) ++
        shortToByteArray(I.item.typeEquip)
      /*for (x <- 0 to I.iemParams.slots){
        if (x < 4){
          buf = buf ++ shortToByteArray(x match {
            case 0 => I.item.slot1.intValue()
            case 1 => I.item.slot2.intValue()
            case 2 => I.item.slot3.intValue()
            case 3 => I.item.slot4.intValue()
          })
        }else{
          buf  = buf ++ intToByteArray(x match {
            case 4 => I.item.others1
            case 5 => I.item.others2
            case 6 => I.item.others3
            case 7 => I.item.others4
            case 8 => I.item.others5
            case 9 => I.item.others6
          })
        }
      }*/
    })
    println(items.length)
    val arr = shortToByteArray(164) ++
      shortToByteArray(It.length * 42 + 4) ++
      buf
    Answer(ByteString(arr))
  }

  def pocket554Answer(CharacteId: Long, walk_speed: Int, options: Int,
                      options1: Int, options2: Int, jobid: Int,
                      tochka: Int, viewWeapon: Int, viewShield: Int,
                      viewHead: Int, hairColor: Int, clothesColor: Int,
                      guildEmblem: Int, manner: Int, options3: Long,
                      karma: Short, sex: Short, x: Short, y: Short, dir: Short,
                      isGM: Short, isDead: Short, baseLevel: Int): Answer = {

    val arr = shortToByteArray(554) ++
      intToByteArray(CharacteId) ++
      shortToByteArray(walk_speed) ++
      shortToByteArray(options) ++
      shortToByteArray(options1) ++
      intToByteArray(options2) ++
      shortToByteArray(jobid) ++
      shortToByteArray(tochka) ++
      shortToByteArray(viewWeapon) ++
      shortToByteArray(viewShield) ++
      stringToByteArray("", 2) ++
      shortToByteArray(viewHead) ++
      stringToByteArray("", 2) ++
      shortToByteArray(hairColor) ++
      shortToByteArray(clothesColor) ++
      stringToByteArray("", 6) ++
      shortToByteArray(guildEmblem) ++
      shortToByteArray(manner) ++
      intToByteArray(options3) ++
      byteToByteArray(karma) ++
      byteToByteArray(sex) ++
      SetEncodedArray3(x, y, dir) ++
      byteToByteArray(isGM) ++
      stringToByteArray("", 1) ++
      byteToByteArray(isDead) ++
      shortToByteArray(baseLevel)
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
      SetEncodedArray6(x, y, x1, y1, sy, sx) ++
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
