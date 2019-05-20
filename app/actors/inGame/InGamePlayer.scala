package app.actors.inGame

import akka.actor.{Actor, ActorSelection, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.{ByteString, Timeout}
import akka.actor.Actor
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
  implicit val timeout: Timeout = Timeout(Duration.create(5, "seconds"))


  override def preStart(): Unit = println("In-game actor born.")

  def receive: PartialFunction[Any, Unit] = {
    case Received(data) => pocketNumber(data) match {
      case 245 =>

        val pData = parsePocket245(data)
        println(pData)
        sender() ! Write(pocket643Answer(pData.mapCharId).data)

        val future = SqlSend ? CordsSearch(pData.mapCharId)
        val slotId = Await.result(future, timeout.duration).asInstanceOf[Cords]

        println(slotId)
        sender() ! Write(pocket115Answer(pData.mapCharId, slotId.x, slotId.y, slotId.dir).data)

      case _ => println
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
  case class Cords(CharacterId: Long, x: Short=0, y: Short=0, dir: Short=0)
}
