package app.actors.preStartGame

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}

class WebServer() extends Actor{
  import Tcp._
  import app.Settings.{TCP_IP, hostPath}
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress(hostPath, TCP_IP._5))


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

  override def postStop(): Unit = println("Web Server Actor died.")

}

