package app.actors

import java.sql.Date
import java.time.{LocalDate, LocalDateTime}

import akka.actor.{Actor, ActorRef, ActorSelection}
import akka.io.Tcp.Write
import akka.util.{ByteString, Timeout}
import app.Settings.ActorPath
import utils.sqlutils.SQLActor.{AddNewCharacter, AddNewPlayer, PlayerLoginStats, SearchProps}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.pattern.ask
import app.PocketLogic.CheckBase
import app.actors.LoginActor.UserInfo
import utils.answers.{CharacterAnswer, LoginAnswer}
import utils.parsing.ParserServ

import scala.collection.mutable.ArrayBuffer

class LoginActor extends Actor
  with ParserServ
  with LoginAnswer
  with CharacterAnswer
  with CheckBase {

  private implicit val newUserData: ArrayBuffer[UserInfo] = new ArrayBuffer[UserInfo]()
  implicit val timeout: Timeout = Timeout(Duration.create(5, "seconds"))
  import LoginActor._


  override def receive: Receive = {
    case LoginData(data, ref) => {
      //получает распарсенную дату, кортеж в виде всех полей
      val parsedData = parsePocket100(data)
      println(parsedData._bytes)
      val info = UserInfo(parsedData.email,parsedData.password,ref)

      //проверяем и добавляем в буфер не зареганых юзеров
      isInfoInMemory(info)
      if (info.login.endsWith("|")) newUserData += info
      //

      if (checkUserEmail(parsedData.email, parsedData.password)) {
        if (parsedData.email.charAt(parsedData.email.length - 1) == '|'){
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

    case NewUserInfo(data, ref) =>
      val parsedData = parsePocket103(data)
      if (parsedData.summ == 0) {
        if (!checkUserNickname(parsedData._nickname)) {
          println("bad nick")
          ref ! Write(pocket110Answer(253).data)
        }
        else {
          val future = SqlSend ? SearchProps(parsedData._nickname,"name", "char_account")
          val result = Await.result(future, timeout.duration).asInstanceOf[String]
          println(result + " --- 1" + parsedData._nickname)
          if (result.equals(parsedData._nickname)) {
            println("ib base!" + result)
            ref ! Write(pocket110Answer(0).data)
          }
          else {
            ref ! Write(pocket110Answer(3).data)
          }
        }
      }
      else if (parsedData.summ == 30){
        val isMale: Int = (parsedData._ismale >> 15) | (0 & 0x7FFF) // пол персонажа
        val job = getJobFract(parsedData._jobid) // фракция и класс

        println(parsedData._jobid + "   " + job)

        val userInfo: UserInfo = getFromUserData(ref)
        implicit val info: UserInfo = UserInfo(userInfo.login.replace("|",""), userInfo.password, userInfo.ref)

        //создаётся новый логин в базе
        val future = SqlSend ? AddNewPlayer(info)
        val result = Await.result(future, timeout.duration).asInstanceOf[Int]

        if (result == -1) Write(pocket110Answer(250).data) // ошибка бд
        else {
          isInfoInMemory(userInfo)
          //создаётся новый чар в базе
          println("Adding new character")

          val characterInfo = CharacterInfo(parsedData._nickname, job.jobId, isMale.shortValue(), job.fraction, 0)

          val future = SqlSend ? AddNewCharacter(info, characterInfo)
          val result = Await.result(future, timeout.duration).asInstanceOf[(Long, Long)]
          val charID = result._1

          if (charID == -1) Write(pocket110Answer(250).data) // ошибка бд
          else {
            //всё ОК - шлём 109й

            val out = pocket109NewChar(charID, job.jobId, isMale, 0, job.fraction, parsedData._nickname)
            ref ! Write(out.data)
          }
        }
      }



    case UserBaseInfo(data, ref) =>
    //val parsedData = parsePocket101(data)
    //println(parsedData.loginAccountId)


    case a@_ => println(a)
  }

  override def postStop(): Unit = println("LoginActor died.")


  def SqlSend: ActorSelection = context.actorSelection (ActorPath ("SQL"))
}

object LoginActor {

  case class CharacterInfo(name: String, jobId: Int, local3: Short, clothesColor: Short, hairColor: Short,slot: Short = 2)

  case class UserInfo(login: String, password: String, ref: ActorRef)

  case class UserBaseInfo(data: ByteString, actorRef: ActorRef)

  case class NewUserInfo(data: ByteString, actorRef: ActorRef)

  case class CheckUserInDB(login: String, pass: String)

  case class LoginData(data: ByteString, actorRef: ActorRef)

  case class LoginAnswerOut(data: ByteString)

}
