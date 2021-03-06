package app

import akka.actor.{ActorSystem, Props}
import akka.stream.ActorMaterializer
import app.actors.preStartGame.{LoginActor, WebServer}
import app.actors.preStartGame.WebServer
import app.actors.inGame.MapActor
import utils.parsing.DataFunc
import utils.sqlutils.SQLActor

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source
import scala.util.control.Breaks

object main extends App with DataFunc{
  println("Hi111111")
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
        val a1 = Integer.valueOf(arr(0)).shortValue()
        val a2 = Integer.valueOf(arr(1)).shortValue()
        val a3 = Integer.valueOf(arr(2)).shortValue()
        val a4 = Integer.valueOf(arr(3)).shortValue()
        val port = Integer.valueOf(arr(4)).shortValue()
        TCP_IP = (a1, a2, a3, a4, port)
      }
      if (adress(0).equals("MAP_IP")) {
        val addr = adress(1)
        val arr = addr.split("\\.")
        val a1 = Integer.valueOf(arr(0)).shortValue()
        val a2 = Integer.valueOf(arr(1)).shortValue()
        val a3 = Integer.valueOf(arr(2)).shortValue()
        val a4 = Integer.valueOf(arr(3)).shortValue()
        val port = Integer.valueOf(arr(4)).shortValue()
        MAP_IP = (a1, a2, a3, a4, port)
      }
      if (adress(0).equals("jdbc")){
        dbName = adress(1)
        nmSQL = adress(2)
        pswSQL = adress(3)
      }
    }
  }catch {
    case  e => e.printStackTrace()
  }

  val tcpServ    = actorSystem.actorOf(Props(classOf[WebServer]), name = "Tcp")
  val loginActor = actorSystem.actorOf(Props(classOf[LoginActor]), name = "Login")
  val sqlActor   = actorSystem.actorOf(Props(classOf[SQLActor]), name = "SQL")
  val mapActor   = actorSystem.actorOf(Props(classOf[MapActor]), name = "Map")
  println(loginActor.path)

}

object Settings{

  var pswSQL:String = _
  var nmSQL:String = _
  var dbName:String = _

  var TCP_IP:(Short,Short,Short,Short,Short) = _
  var MAP_IP:(Short,Short,Short,Short,Short) = _
  lazy val hostPath: String = TCP_IP._1 + "." + TCP_IP._2 + "." + TCP_IP._3 + "." + TCP_IP._4
  lazy val mapHostPath: String = MAP_IP._1 + "." + MAP_IP._2 + "." + MAP_IP._3 + "." + MAP_IP._4
  def ActorPath(path:String) : String = "akka://ActorSystem/user/"+path
  private var xml = "<?xml version=\"1.0\"?>\n<!DOCTYPE cross-domain-policy SYSTEM \n\"http://www.adobe.com/xml/dtds/cross-domain-policy.dtd\">\n<cross-domain-policy>\n"
  xml += "<site-control permitted-cross-domain-policies=\"master-only\"/>\n"
  xml += "<allow-access-from domain=\"*\" to-ports=\"*\"/>\n"
  xml += "</cross-domain-policy>\n"
  val POLICY: String = xml
}
