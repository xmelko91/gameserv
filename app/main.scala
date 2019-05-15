package app

import akka.actor.{ActorSystem, Props}
import akka.stream.ActorMaterializer

import scala.concurrent.ExecutionContextExecutor

object main extends App{

  implicit val actorSystem: ActorSystem                   = ActorSystem("ActorSystem")
  implicit val materializer: ActorMaterializer            = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = actorSystem.dispatcher


  val tcpSerrv = actorSystem.actorOf(Props(classOf[WebServer], "localhost", 2973))
  val loginActor = actorSystem.actorOf(Props(classOf[LoginActor]), name = "Login")
  println(loginActor.path)


}

object Settings{
  def ActorPath(path:String) : String = "akka://ActorSystem/user/"+path
  private var xml = "<?xml version=\"1.0\"?>\n<!DOCTYPE cross-domain-policy SYSTEM \n\"http://www.adobe.com/xml/dtds/cross-domain-policy.dtd\">\n<cross-domain-policy>\n"
  xml += "<site-control permitted-cross-domain-policies=\"master-only\"/>\n"
  xml += "<allow-access-from domain=\"*\" to-ports=\"*\"/>\n"
  xml += "</cross-domain-policy>\n"
  val POLICY = xml
}
