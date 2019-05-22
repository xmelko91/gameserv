package utils.parsing

import akka.http.scaladsl.model.DateTime

trait MathUtils extends DataFunc {

  def dateToArray: DateTime => Array[Byte] = (time: DateTime) => intToByteArray((time.hour * 360 + time.minute * 60 + time.second).longValue())

}
