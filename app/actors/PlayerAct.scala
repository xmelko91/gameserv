package app.actors

import akka.actor.{Actor, ActorRef, ActorSelection, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.{ByteString, Timeout}
import app.Settings
import app.Settings.ActorPath
import akka.pattern.ask
import utils.answers.LoginAnswer
import utils.parsing.ParserServ
import scala.concurrent.duration.Duration

class PlayerAct extends Actor
  with ParserServ
  with LoginAnswer{

  import LoginActor._
  implicit val timeout: Timeout = new Timeout(Duration.create(5, "seconds"))


  def receive: PartialFunction[Any, Unit] = {
    //Проверка на полси - отдаёт пустой пакет обратно
    case Received(data) if data.utf8String.startsWith("<policy-file-request/>") =>
      val xml = Settings.POLICY
      val bytes = new Array[Byte](xml.getBytes().length + 1)
      for (x <-xml.getBytes().indices){
        bytes(x) = xml.getBytes()(x)
      }
      sender() ! Write(ByteString.apply(bytes))
    //при получении даты отправляет её в LoginActor
    case Received(data) => pocketNumber(data) match {
      //здесь по номеру пакета свичимся на нужную логику

      case 100 =>
        println("GET")
        LoginSend ! LoginData(data, sender())
      case 101 =>
        println("101 is here")
        LoginSend ! UserBaseInfo(data, sender())
      case 103 =>
        println("103 here")
        LoginSend ! NewUserInfo(data, sender())

      case _ => println("Необработаный пакет № " + pocketNumber(data))
    }


    //Конец жизни Актора
    case PeerClosed     => {
      context.stop(self)
      println("Player actor died")
      self ! PoisonPill
    }
  }

  def LoginSend: ActorSelection = context.actorSelection (ActorPath ("Login"))
  def SqlSend: ActorSelection = context.actorSelection (ActorPath ("SQL"))
}
