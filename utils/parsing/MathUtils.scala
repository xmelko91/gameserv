package utils.parsing

import akka.http.scaladsl.model.DateTime
import app.actors.inGame.InGamePlayer.{CalculatedStats, CharBaseStats}
import utils.sqlutils.MapSQL.ItemsSet

trait MathUtils extends DataFunc {

  def dateToArray: DateTime => Array[Byte] = (time: DateTime) => intToByteArray((time.hour * 360 + time.minute * 60 + time.second).longValue())
  val DIRS: Array[Short] = Array(0, 7, 6, 5, 4, 3, 2, 1)

  def GetEncoded3(_arg_1:Array[Short]): Cords3 = {
    val x = ((_arg_1(0) << 2) | (_arg_1(1) >> 6)) & 0x03FF
    val y = ((_arg_1(1) << 4) | (_arg_1(2) >> 4)) & 0x03FF
    val dir:Short = DIRS(_arg_1(2) & 0x0F)

    Cords3(x.shortValue(), y.shortValue(), dir)
  }

  def GetEncoded6(_arg_1:Array[Byte]): Cords6 =  {
    val x = ((_arg_1{0} << 2) | (_arg_1{1} >> 6)) & 0x03FF
    val y = ((_arg_1{1} << 4) | (_arg_1{2} >> 4)) & 0x03FF
    val x1 = ((_arg_1{2} << 6) | (_arg_1{3} >> 2)) & 0x03FF
    val y1 = ((_arg_1{3} << 8) | _arg_1{4}) & 0x03FF
    val sx0 = (_arg_1{5} >> 4) & 0x0F
    val sy0 = _arg_1{5} & 0x0F

    Cords6(x.shortValue(), y.shortValue(), x1.shortValue(), y1.shortValue(), sy0.shortValue(), sx0.shortValue())
  }

  def SetEncodedArray3(x: Short, y: Short, dir: Short): Array[Byte] = {
    val _local_1: Array[Byte] = new Array[Byte](3)
    _local_1(0) = (x.byteValue() >> 2).byteValue()
    _local_1(1) = (x.byteValue() << 6 | (y.byteValue() >> 4) & 0x3F).byteValue()
    _local_1(2) = (y.byteValue() << 4 | DIRS(dir) & 0x0F).byteValue()
    _local_1
  }

  def SetEncodedArray6(x: Short, y: Short, x1: Short, y1: Short, sy0: Short, sx0: Short): Array[Byte] = {
    val _local_1:Array[Byte] = new Array[Byte](6)
    _local_1(0) = (x.byteValue() >> 2).byteValue()
    _local_1(1) = (x.byteValue() << 6 | (y.byteValue() >> 4) & 0x3F).byteValue()
    _local_1(2) = (y.byteValue() << 4 | (x1.byteValue() >> 6) & 0x0F).byteValue()
    _local_1(3) = (x1.byteValue() << 2 | (y1.byteValue() >> 8) & 0x03).byteValue()
    _local_1(4) = y1.byteValue()
    _local_1(5) = (sx0.byteValue() << 4 | sy0.byteValue() & 0x0F).byteValue()
    _local_1
  }

  def encodeCord(x: Short): Short =
  {
    (x & 0x03FF).shortValue()
  }

  def encodeDir(dir: Short): Short =
  {
    (dir & 0x0F).shortValue()
  }

  def StatsCalculating(stats: CharBaseStats, playerItems: Array[ItemsSet]): CalculatedStats = {
    val statsCount = stats.statCout // количество свободных очков
    val strA = upgradePrice(stats.str)
    val agiA = upgradePrice(stats.agi)
    val dexA = upgradePrice(stats.dex)
    val intA = upgradePrice(stats.int)
    val vitA = upgradePrice(stats.vit)
    val lukA = upgradePrice(stats.luk)
    val atk1 = 0
    val atk2 = 10
    val mAtkMin = 0
    val mAtkMax = 10
    val def1 = 0
    val def2 = 10
    val mdef1 = 0
    val mdef2 = 10
    val hit = 50
    val flee1 = 0
    val flee2 = 10
    val critical = 10
    val karma = 10
    val manner = 1


    CalculatedStats(statsCount, stats.str, strA,
      stats.agi, agiA, stats.vit, vitA, stats.int, intA, stats.dex, dexA, stats.luk, lukA,
      atk1, atk2, mAtkMax, mAtkMin, def1, def2, mdef1, mdef2, hit, flee1, flee2, critical, karma, manner)
  }

  def upgradePrice(v: Short):Short = ((v + 9) / 10).shortValue()



  case class Cords3(x: Short, y: Short, dir: Short)
  case class Cords6(x: Short, y: Short, x1: Short, y1: Short, sy0: Short, sx0: Short)

}
