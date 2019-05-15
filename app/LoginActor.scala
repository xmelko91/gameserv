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
      val out = pocket100Answer(
        105, 79,111,112,113,1,1,100,22243,20,0,
        127, 0, 0, 1, 2973, "darkness01", 20, 66, 55, 44)
      println(out.data + ref.path.name)
      ref ! Write(out.data)
    case a@_ => println(a)
  }
}

object LoginActor{
  case class LoginData(data: ByteString, actorRef: ActorRef)
  case class LoginAnswerOut(data:ByteString)
}
