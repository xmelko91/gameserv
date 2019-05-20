package app.PocketLogic

import scala.util.matching.Regex
import RegexUtils._
import java.security.MessageDigest

import akka.actor.ActorRef
import app.actors.preStartGame.LoginActor.{NewUserId, UserInfo}

import scala.collection.mutable.ArrayBuffer


trait CheckBase {
  //Здесь прописывает функции проверок

  def checkUserEmail: (String, String) => Boolean = (email : String, password: String) => (MailReg matches email) && (PassReg matches password)

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

  def getFromUserId(ref: ActorRef)(implicit arr: ArrayBuffer[NewUserId]):NewUserId = {
    arr.foreach(value =>
      if (value.ref.equals(ref)) return value)
    null
  }

  def getFromUserIdS(ref: ActorRef, charId: Long)(implicit arr: ArrayBuffer[NewUserId]):NewUserId = {
    arr.foreach(value =>
      if (value.userId == charId && ref.equals(value.ref)) return value)
    null
  }

  def checkUserInB(ref: ActorRef)(implicit arr: ArrayBuffer[NewUserId]):Boolean = {
    arr.foreach(value =>
      if (value.ref.equals(ref)) return true)
    false
  }

  def md5(s: Array[Byte]): Array[Byte] = {
    MessageDigest.getInstance("MD5").digest(s)
  }
  def md5(s: String): Array[Byte] = {
    MessageDigest.getInstance("MD5").digest(s.getBytes())
  }

}

object RegexUtils {

  val MailReg: Regex = """[a-zA-Z0-9|]{5,25}""".r
  val PassReg: Regex = """[a-zA-Z0-9]{16}""".r
  val OnlyRus: Regex = """[а-яА-Я0-9ёЁ]{3,25}""".r
  val OnlyEng: Regex = """[a-zA-Z0-9]{3,25}""".r

  implicit class RichRegex(val underlying: Regex) extends AnyVal {
    def matches(s: String): Boolean = underlying.pattern.matcher(s).matches
  }
}

