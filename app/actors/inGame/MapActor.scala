package app.actors.inGame

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.io.{IO, Tcp}
import akka.util.Timeout
import utils.sqlutils.MapSQL

import scala.concurrent.duration.Duration

class MapActor extends Actor{

  import MapActor._
  import Tcp._
  import app.Settings.{MAP_IP, mapHostPath}
  import context.system

  IO(Tcp) ! Bind(self, new InetSocketAddress(mapHostPath, MAP_IP._5))

  implicit val timeout: Timeout = new Timeout(Duration.create(5, "seconds"))


  override def preStart(): Unit = {
    context.actorOf(Props(classOf[MapSQL]), name = "MapSQL")
  }

  override def receive: Receive = {
    case b @ Bound(localAddress) =>
      context.parent ! b

    case CommandFailed(_: Bind) => context.stop(self)

    case c @ Connected(remote, local) =>
      val handler = context.actorOf(Props(classOf[InGamePlayer]))
      val connection = sender()
      println("registred in-game player " + remote.getAddress.toString + " : " + remote.getPort.toString)
      connection ! Register(handler)
    case _ => print("asdsasd")
  }
}

object MapActor{
  case class a()
}
