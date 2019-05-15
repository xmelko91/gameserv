package app

import akka.actor.{ActorSystem, Props}
import akka.stream.ActorMaterializer
import app.actors.{LoginActor, WebServer}
import utils.sqlutils.SQLActor

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source
import scala.util.control.Breaks

object main extends App{
  import Settings._

  implicit val actorSystem: ActorSystem                   = ActorSystem("ActorSystem")
  implicit val materializer: ActorMaterializer            = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher

  try {
    val fis = Source.fromResource("Config.txt").getLines()

    for (x <- fis) {
      val adress = x.split("\\s+")
      if (adress(0).equals("TCP_IP")) {
        val addr = adress(1)
        val arr = addr.split("\\.")
        val a1 = Integer.valueOf(arr(0)).byteValue()
        val a2 = Integer.valueOf(arr(1)).byteValue()
        val a3 = Integer.valueOf(arr(2)).byteValue()
        val a4 = Integer.valueOf(arr(3)).byteValue()
        val port = Integer.valueOf(arr(4)).shortValue()
        TCP_IP = (a1, a2, a3, a4, port)
      }
      Breaks.break()
    }
  }catch {
    case  e => e.printStackTrace()
  }

  val tcpServ = actorSystem.actorOf(Props(classOf[WebServer], "localhost", TCP_IP._5.intValue()))
  val loginActor = actorSystem.actorOf(Props(classOf[LoginActor]), name = "Login")
  val sqlActor = actorSystem.actorOf(Props(classOf[SQLActor]), name = "SQL")
  println(loginActor.path)

}

object Settings{
  var TCP_IP:(Byte,Byte,Byte,Byte,Short) = _
  def ActorPath(path:String) : String = "akka://ActorSystem/user/"+path
  private var xml = "<?xml version=\"1.0\"?>\n<!DOCTYPE cross-domain-policy SYSTEM \n\"http://www.adobe.com/xml/dtds/cross-domain-policy.dtd\">\n<cross-domain-policy>\n"
  xml += "<site-control permitted-cross-domain-policies=\"master-only\"/>\n"
  xml += "<allow-access-from domain=\"*\" to-ports=\"*\"/>\n"
  xml += "</cross-domain-policy>\n"
  val POLICY = xml
}
