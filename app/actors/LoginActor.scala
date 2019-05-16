package app.actors

import java.time.LocalDate

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp.Write
import akka.util.{ByteString, Timeout}
import app.Settings.ActorPath
import utils.sqlutils.SQLActor.PlayerLoginStats

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.pattern.ask
import app.PocketLogic.CheckBase
import utils.answers.LoginAnswer
import utils.parsing.ParserServ

class LoginActor extends Actor
  with ParserServ
  with LoginAnswer
  with CheckBase{

  import LoginActor._


  implicit val timeout = Timeout(Duration.create(5, "seconds"))

  override def receive: Receive = {
    case LoginData(data, ref) =>
      //получает распарсенную дату, кортеж в виде всех полей
      val parsedData = parsePocket100(data)

      if (checkUserEmail(parsedData.email, parsedData.password)) {
        //запрос акка из БД
        val future = context.actorSelection(ActorPath("SQL")) ? CheckUserInDB(parsedData.email, parsedData.password)
        val result = Await.result(future, timeout.duration).asInstanceOf[PlayerLoginStats]

        result match {
          case data if data.loginId1 == -1 =>
            println("False to find login id")
            ref ! Write(pocket106Answer(0, "Account does not exists!").data)
          case data if data.banned.toLocalDate.compareTo(LocalDate.now()) > 0 =>
            println("Acc is banned")
            ref ! Write(pocket106Answer(6, data.banned.toString).data)
          case data =>
            val out = pocket105Answer(
            Size = 79, data, 22243, "darkness01", 20, 66, 55, 44)
            println(out.data + ref.path.name)
            ref ! Write(out.data)
        }
      } else {
        ref ! Write(pocket106Answer(3, "Wrong Login or Password!").data)
      }


    case a@_ => println(a)
  }
}

object LoginActor {

  case class CheckUserInDB(login: String, pass: String)

  case class LoginData(data: ByteString, actorRef: ActorRef)

  case class LoginAnswerOut(data: ByteString)

}
