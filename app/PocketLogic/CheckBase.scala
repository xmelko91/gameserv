package app.PocketLogic

import scala.util.matching.Regex
import RegexUtils._
import java.security.MessageDigest

import akka.actor.ActorRef
import app.actors.LoginActor.UserInfo

import scala.collection.mutable.ArrayBuffer


trait CheckBase {
  //Здесь прописывает функции проверок

  def checkUserEmail = (email : String, password: String) => (MailReg matches email) && (PassReg matches password)

  def checkUserNickname: String => Boolean = (name:String) => {
    (OnlyEng matches name) || (OnlyRus matches name)
  }

  def isInfoInMemory(info: UserInfo)(implicit arr: ArrayBuffer[UserInfo]) : Boolean ={
    arr.foreach{value =>
      if (info.login.startsWith(value.login)) {
        arr -= value
        return true
      }
    }
    false
  }

  def getFromUserData(ref: ActorRef)(implicit arr: ArrayBuffer[UserInfo]):UserInfo = {
    arr.foreach(value =>
    if (value.ref.equals(ref)) return value)
    null
  }

  def md5(s: Array[Byte]) = {
    MessageDigest.getInstance("MD5").digest(s)
  }
  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes())
  }

}

object RegexUtils {

  val MailReg = """[a-zA-Z0-9|]{5,25}""".r
  val PassReg = """[a-zA-Z0-9]{16}""".r
  val OnlyRus = """[а-яА-Я0-9]{3,25}""".r
  val OnlyEng = """[a-zA-Z0-9]{3,25}""".r

  implicit class RichRegex(val underlying: Regex) extends AnyVal {
    def matches(s: String) = underlying.pattern.matcher(s).matches
  }
}

