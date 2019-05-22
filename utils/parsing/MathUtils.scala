package utils.parsing

import akka.http.scaladsl.model.DateTime
import akka.util.ByteString

trait MathUtils extends DataFunc {

  def dateToArray: DateTime => Array[Byte] = (time: DateTime) => intToByteArray((time.hour * 360 + time.minute * 60 + time.second).longValue())

  def setEncode6(x: Short, y: Short, x1:Short, y1:Short, d: Short = 0, d1: Short = 6) = {
    val arr = byteToByteArray((x >> 2).byteValue()) ++
      byteToByteArray(((x << 6) | ((y >> 4) & 0x3F)).byteValue()) ++
      byteToByteArray(((y << 4) | ((x1 >> 6) & 0x0F)).byteValue()) ++
      byteToByteArray(((x1 << 2) | ((y1 >> 8) & 0x03)).byteValue()) ++
      byteToByteArray(y1.byteValue()) ++
      byteToByteArray(((d << 4) | (d1 & 0x0F)).byteValue())
    arr
  }

}
