package utils.parsing

import java.io.ByteArrayInputStream

import akka.util.ByteString

trait InGameParse extends DataFunc {






  //читает первый Шорт
  def pocketNumber(data:ByteString): Int = {
    val buf = new ByteArrayInputStream(data.toArray)
    val out = readUShort(buf.readNBytes(2))
    buf.close()
    out
  }
}
