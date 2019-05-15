package app.actors

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}

class WebServer(address: String, port: Int) extends Actor{
  import Tcp._
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress(address, port))


  def receive: PartialFunction[Any, Unit] = {
    case b @ Bound(localAddress) =>
      context.parent ! b

    case CommandFailed(_: Bind) => context.stop(self)

    case c @ Connected(remote, local) =>
      val handler = context.actorOf(Props(classOf[PlayerAct]))
      val connection = sender()
      println("registred " + remote.getAddress.toString + " : " + remote.getPort.toString)
      connection ! Register(handler)
    case _ => print("asdsasd")
  }


}

