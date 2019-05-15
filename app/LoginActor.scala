package app

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp.Write
import akka.util.ByteString
import utils.LoginAnswer._


class LoginActor extends Actor{
  import LoginActor._
  import utils.ParserServ._
  override def receive: Receive = {
    case LoginData(data, ref) =>
      //получает распарсенную дату, кортеж в виде всех полей
      val parsedData = parseLogIn(data)
      val out = composeArray(105, 47,11,2704328,12034,1,1,1,1,10,20)
      println(out.data + ref.path.name)
      ref ! Write(out.data)
    case a@_ => println(a)
  }
}

object LoginActor{
  case class LoginData(data: ByteString, actorRef: ActorRef)
  case class LoginAnswerOut(data:ByteString)
}
