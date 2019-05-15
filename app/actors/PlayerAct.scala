package app.actors

import akka.actor.{Actor, ActorRef, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.{ByteString, Timeout}
import app.Settings
import app.Settings.ActorPath
import akka.pattern.ask

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class PlayerAct extends Actor{

  import utils.sqlutils.SQLActor._
  import LoginActor._
  import utils.answers.LoginAnswer._
  import utils.parsing.ParserServ._

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
        val future = context.actorSelection(ActorPath("SQL")) ? SearchProps(1, "idPlayer" ,"Player")
        val result = Await.result(future, timeout.duration).asInstanceOf[Int]
        println("id of admin is "+ result)
        context.actorSelection (ActorPath ("Login") ) ! LoginData (data, sender ())

      case 101 =>
        println("whs mzfk")
        sender() ! pocket106Answer(1, "banned")

      case _ => println("Необработаный пакет № " + pocketNumber(data))
    }


    //Конец жизни Актора
    case PeerClosed     => {
      context.stop(self)
      println("actor died")
      self ! PoisonPill
    }
  }
}
