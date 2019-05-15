package app.actors

import akka.actor.{Actor, ActorRef, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.ByteString
import app.Settings
import app.Settings.ActorPath

class PlayerAct extends Actor{

  import LoginActor._
  import utils.parsing.ParserServ._
  var boss:ActorRef = null


  override def preStart(): Unit ={
  }

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
        println (data + sender().path.name)
        context.actorSelection (ActorPath ("Login") ) ! LoginData (data, sender ())


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