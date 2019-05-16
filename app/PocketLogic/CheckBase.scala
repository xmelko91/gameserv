package app.PocketLogic

import scala.util.matching.Regex
import RegexUtils._
import java.security.MessageDigest


trait CheckBase {
  //Здесь прописывает функции проверок

  def checkUserEmail = (email : String, password: String) => if ((MailReg matches email) && (PassReg matches password)) true else false

  def md5(s: Array[Byte]) = {
    MessageDigest.getInstance("MD5").digest(s)
  }
  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes())
  }

}

object RegexUtils {

  val MailReg = """[a-z0-9]{5,25}""".r
  val PassReg = """[a-z0-9]{16}""".r

  implicit class RichRegex(val underlying: Regex) extends AnyVal {
    def matches(s: String) = underlying.pattern.matcher(s).matches
  }
}