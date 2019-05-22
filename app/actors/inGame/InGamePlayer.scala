package app.actors.inGame

import akka.actor.{Actor, ActorSelection, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.{ByteString, Timeout}
import akka.actor.Actor
import akka.http.scaladsl.model.DateTime
import app.Settings.ActorPath
import utils.answers.InGameAnswer
import utils.parsing.InGameParse
import akka.pattern.ask
import app.actors.inGame.InGamePlayer.Cords
import utils.sqlutils.MapSQL.CordsSearch

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class InGamePlayer extends Actor with InGameParse with InGameAnswer{

  var charId:Long = 0
  var gold: Long = 0
  var timer:Long = 0
  implicit val timeout: Timeout = Timeout(Duration.create(5, "seconds"))


  override def preStart(): Unit = println("In-game actor born.")

  def receive: PartialFunction[Any, Unit] = {
    case Received(data) => pocketNumber(data) match {
      case 125 =>
        println("125 p")
        sender() ! Write(pocket189Answer().data)

      case 137 =>
        println("137 p")
        val pData = parsePocket137(data)
        println(pData)

      case 245 =>
        val pData = parsePocket245(data)
        this.timer = pData.timer
        this.charId = pData.mapCharId
        println(pData)
        sender() ! Write(pocket643Answer(pData.mapCharId).data)

        val future = SqlSend ? CordsSearch(pData.mapCharId)
        val cords = Await.result(future, timeout.duration).asInstanceOf[Cords]
        this.gold = cords.gold

        println(cords)
        sender() ! Write(pocket115Answer(pData.mapCharId, cords.x, cords.y, cords.dir).data)

      case 278 =>
        println("278 here")
        val pData = parsePocket278(data)
        val date = DateTime.now
        sender() ! Write(pocket1059Answer(date).data)

      case 1048 =>
        println("1048 here " + gold)
        sender() ! Write(pocketGold(gold).data)

      case 1049 =>
        //quest logic
        //sender() ! Write(pocket1025Answer())

      case 1058 =>
        println("1058 here")
        sender() ! Write(pocket127Answer().data)

      case _ => println(pocketNumber(data))
    }


    case PeerClosed => {
      context.stop(self)
      println("In-game actor died")
      self ! PoisonPill
    }
  }





  def SqlSend: ActorSelection = context.actorSelection(ActorPath("Map/MapSQL"))
}

object InGamePlayer{
  case class Cords(CharacterId: Long, x: Short=0, y: Short=0, dir: Short=0, gold : Long)
}
