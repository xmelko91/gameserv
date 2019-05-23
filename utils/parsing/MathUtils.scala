package utils.parsing

import akka.http.scaladsl.model.DateTime
import akka.util.ByteString

trait MathUtils extends DataFunc {

  def dateToArray: DateTime => Array[Byte] = (time: DateTime) => intToByteArray((time.hour * 360 + time.minute * 60 + time.second).longValue())
  val DIRS: Array[Short] = Array(0, 7, 6, 5, 4, 3, 2, 1)

  def GetEncoded3(_arg_1:Array[Byte]): Cords3 = {
    val x = ((_arg_1(0) << 2) | (_arg_1(1) >> 6)) & 0x03FF
    val y = ((_arg_1(1) << 4) | (_arg_1(2) >> 4)) & 0x03FF
    val dir:Short = DIRS(_arg_1{2} & 0x0F)

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

  def SetEncodedArray3(x: Short, y: Short, dir: Short): Array[Short] = {
    val _local_1: Array[Short] = new Array[Short](3)
    _local_1(0) = (x >> 2).shortValue()
    _local_1(1) = (x << 6 | (y >> 4) & 0x3F).shortValue()
    _local_1(2) = (y << 4 | DIRS(dir) & 0x0F).shortValue()
    _local_1
  }

  def SetEncodedArray6(x: Short, y: Short, x1: Short, y1: Short, sy0: Short, sx0: Short): Array[Short] = {
    val _local_1:Array[Short] = new Array[Short](6)
    _local_1(0) = (x >> 2).shortValue()
    _local_1(1) = (x << 6 | (y >> 4) & 0x3F).shortValue()
    _local_1(2) = (y << 4 | (x1 >> 6) & 0x0F).shortValue()
    _local_1(3) = (x1 << 2 | (y1 >> 8) & 0x03).shortValue()
    _local_1(4) = y1.shortValue()
    _local_1(5) = (sx0 << 4 | sy0 & 0x0F).shortValue()
    _local_1
  }

  def encodeCord(x: Long): Long =
  {
    x & 0x03FF
  }

  def encodeDir(dir: Long): Long =
  {
    dir & 0x0F
  }


  case class Cords3(x: Short, y: Short, dir: Short)
  case class Cords6(x: Short, y: Short, x1: Short, y1: Short, sy0: Short, sx0: Short)

}
