package app.actors.preStartGame

import java.time.{LocalDate, LocalDateTime}

import akka.actor.{Actor, ActorRef, ActorSelection}
import akka.io.Tcp.Write
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import app.PocketLogic.CheckBase
import app.Settings.ActorPath
import app.actors.preStartGame.LoginActor.{NewUserId, UserInfo}
import utils.answers.CharacterAnswer.CharStats
import utils.answers.{CharacterAnswer, LoginAnswer}
import utils.parsing.ParserServ
import utils.sqlutils.SQLActor._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class LoginActor extends Actor
  with ParserServ
  with LoginAnswer
  with CharacterAnswer
  with CheckBase {

  private implicit val newUserData: ArrayBuffer[UserInfo] = new ArrayBuffer[UserInfo]()
  private implicit val UserId: ArrayBuffer[NewUserId] = new ArrayBuffer[NewUserId]()
  implicit val timeout: Timeout = Timeout(Duration.create(5, "seconds"))

  import LoginActor._


  override def receive: Receive = {
    case LoginData(data, ref) => {
      //получает распарсенную дату, кортеж в виде всех полей
      val parsedData = parsePocket100(data)
      println(parsedData._bytes)
      val info = UserInfo(parsedData.email, parsedData.password, ref)

      //проверяем и добавляем в буфер не зареганых юзеров
      isInfoInMemory(info)
      if (info.login.endsWith("|")) newUserData += info
      //

      if (checkUserEmail(parsedData.email, parsedData.password)) {
        if (parsedData.email.charAt(parsedData.email.length - 1) == '|') {
          ref ! Write(pocket110Answer(2).data)
        }
        else {
          //запрос акка из БД
          val future = context.actorSelection(ActorPath("SQL")) ? CheckUserInDB(parsedData.email, parsedData.password)
          val result = Await.result(future, timeout.duration).asInstanceOf[PlayerLoginStats]

          result match {
            case data if data.loginId1 == -1 =>
              println("False to find login id")
              ref ! Write(pocket106Answer(0, LocalDateTime.now().toString).data)
            case data if data.banned.toLocalDate.compareTo(LocalDate.now()) > 0 =>
              println("Acc is banned")
              ref ! Write(pocket106Answer(6, data.banned.toString).data)
            case data =>
              val out = pocket105Answer(
                Size = 79, data, 22243, "darkness01", 20, 66, 55, 44)
              ref ! Write(out.data)
          }
        }
      } else {
        ref ! Write(pocket106Answer(3, LocalDateTime.now().toString).data)
      }
    }

    case NewUserInfo(data, ref) => {
      val parsedData = parsePocket103(data)
      if (parsedData.summ == 0) {
        if (!checkUserNickname(parsedData._nickname)) {
          println("bad nick")
          ref ! Write(pocket110Answer(253).data)
        }
        else {
          val future = SqlSend ? SearchProps(parsedData._nickname, "name", "char_account")
          val result = Await.result(future, timeout.duration).asInstanceOf[String]
          if (result.equals(parsedData._nickname)) {
            println("ib base!" + result)
            ref ! Write(pocket110Answer(0).data)
          }
          else {
            ref ! Write(pocket110Answer(3).data)
          }
        }
      }
      else if (parsedData.summ == 30) {
        val isMale: Int = (parsedData._ismale >> 15) | (0 & 0x7FFF)// пол персонажа
        val job = getJobFract(parsedData._jobid) // фракция и класс

        println(parsedData._jobid + "   " + job)

        val userInfo: UserInfo = getFromUserData(ref)
        implicit var info: UserInfo = null

        var result:Long = 0
        var user: NewUserId = null
        //проверяем логин в базе
        if ((user = getFromUserId(ref)) != null) {
           info = UserInfo("1","1",ref)
        }else{
          //создаётся новый логин в базе
          info = UserInfo(userInfo.login.replace("|", ""), userInfo.password, userInfo.ref)
          val future = SqlSend ? AddNewPlayer(info)
          result = Await.result(future, timeout.duration).asInstanceOf[Long]
        }
        if (result == -1) Write(pocket110Answer(250).data) // ошибка бд
        else {
          //isInfoInMemory(userInfo)
          //создаётся новый чар в базе
          println("Adding new character")

          val characterInfo = CharacterInfo(parsedData._nickname, job.jobId, isMale.shortValue(), job.fraction, 1, parsedData._slotid)

          val future = SqlSend ? AddNewCharacter(info, characterInfo, user.userId)
          val result = Await.result(future, timeout.duration).asInstanceOf[(Long, Long)]
          val loginID = result._2
          val charID = result._1

          UserId += NewUserId(loginID, charID, ref)

          if (charID == -1) Write(pocket110Answer(250).data) // ошибка бд
          else {
            //всё ОК - шлём 109й

            val out = pocket109NewChar(charID, job.jobId, isMale, 0, job.fraction, parsedData._nickname)
            ref ! Write(out.data)
          }
        }
      }
    }

    case UserBaseInfo(data, ref) => {
      println(data)
      val loginId = parsePocket101(data)

      val future = SqlSend ? AllChars(loginId.loginAccountId)
      val result = Await.result(future, timeout.duration).asInstanceOf[Array[CharStats]]

      ref ! Write(ByteString(intToByteArray(loginId.loginAccountId)))
      UserId += NewUserId(loginId.loginAccountId, -1, ref)
      ref ! Write(pocket107Answer(result).data)

    }
    //val parsedData = parsePocket101(data)
    //println(parsedData.loginAccountId)

    case Slot(data, ref) => {
      var slotId:Long = 0

      if (data.length > 2){
        val slot = parsePocket102(data)._slotid
        val user = getFromUserId(ref).userId

        val future = SqlSend ? AllChars(user, slot)
        slotId = Await.result(future, timeout.duration).asInstanceOf[Long]

      }
      else {
        println(data.length)
        slotId = getFromUserId(ref).charId
      }
      ref ! Write(pocket113Answer(slotId, "city00").data)
    }

    case ChangeNick(stage, data, ref) => {
      var nick = ""
      var charId:Long = 0
      if (stage == 1) {
        val parsedData = parsePocket653(data)
        nick = parsedData.NICKNAME
        charId = parsedData.ChacharacterID
        println(parsedData)

        val future = SqlSend ? SearchProps(parsedData.NICKNAME, "name", "char_account")
        val result = Await.result(future, timeout.duration).asInstanceOf[String]

        if (!result.equals("")) {
          ref ! Write(pocket654Answer(0).data)
        } else {
          ref ! Write(pocket654Answer(1).data)
        }
      }
      else if (stage == 2){
        val parsedData = parsePocket655(data).ChacharacterID

        if (!checkUserNickname(nick)){
          ref ! Write(pocket656Answer(2).data)
        }else{
          //тут добавление в бд допилисть
          ref ! Write(pocket656Answer(0).data)
        }
      }
    }

    case a@_ => println(a + " " + context.system.name)
  }

  override def postStop(): Unit = println("LoginActor died.")


  def SqlSend: ActorSelection = context.actorSelection(ActorPath("SQL"))
}

object LoginActor {

  case class ChangeNick(stage: Int, data: ByteString, actorRef: ActorRef)

  case class Slot(slot: ByteString, ref: ActorRef)

  case class CharacterInfo(name: String, jobId: Int, local3: Short, clothesColor: Short, hairColor: Short, slot: Short)

  case class UserInfo(login: String, password: String, ref: ActorRef)

  case class NewUserId(userId: Long, charId: Long, ref: ActorRef)

  case class UserBaseInfo(data: ByteString, actorRef: ActorRef)

  case class NewUserInfo(data: ByteString, actorRef: ActorRef)

  case class CheckUserInDB(login: String, pass: String)

  case class LoginData(data: ByteString, actorRef: ActorRef)

  case class LoginAnswerOut(data: ByteString)

}
