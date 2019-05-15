package app.actors

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp.Write
import akka.util.{ByteString, Timeout}
import app.Settings.ActorPath
import utils.answers.LoginAnswer._
import utils.sqlutils.SQLActor.{PlayerLoginStats, SearchProps}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.pattern.ask

class LoginActor extends Actor{
  import LoginActor._
  import app.PocketLogic.CheckBase._
  import utils.parsing.ParserServ._
  implicit val timeout = Timeout(Duration.create(5, "seconds"))

  override def receive: Receive = {
    case LoginData(data, ref) =>
      //получает распарсенную дату, кортеж в виде всех полей
      val parsedData = parsePocket100(data)
      println("data " + parsedData.email + "  " + parsedData.password)
      checkUserEmail(parsedData.email)

      //запрос акка из БД
      val future = context.actorSelection(ActorPath("SQL")) ? CheckUserInDB(parsedData.email, parsedData.password)
      val result = Await.result(future, timeout.duration).asInstanceOf[PlayerLoginStats]

      //здесь проверку запилить
      //if (result.loginId1 == -1) возвращаем эррор

      val out = pocket105Answer(
         Size = 79, result,22243,"darkness01", 20, 66, 55, 44)
      println(out.data + ref.path.name)
      ref ! Write(out.data)


    case a@_ => println(a)
  }
}

object LoginActor{
  case class CheckUserInDB(login: String, pass: String)
  case class LoginData(data: ByteString, actorRef: ActorRef)
  case class LoginAnswerOut(data:ByteString)
}
