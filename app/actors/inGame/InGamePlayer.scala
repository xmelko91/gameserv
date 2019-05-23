package app.actors.inGame

import akka.actor.{Actor, ActorRef, ActorSelection, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.{ByteString, Timeout}
import akka.http.scaladsl.model.DateTime
import app.Settings.ActorPath
import utils.answers.InGameAnswer
import utils.parsing.InGameParse
import akka.pattern.ask
import app.actors.inGame.InGamePlayer.{Cords, PlayerMessage}
import utils.sqlutils.MapSQL.CordsSearch

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class InGamePlayer extends Actor with InGameParse with InGameAnswer {


  var player: ActorRef = _
  var nickName: String = ""
  var race: Short = 0
  var premiumType: Short = 0
  var baseLvl: Int = 0
  var isGM: Short = 0
  var charId: Long = 0
  var gold: Long = 0
  var timer: Long = 0
  var dir: Short = 0
  var x: Short = 0
  var y: Short = 0
  var map: String = ""
  implicit val timeout: Timeout = Timeout(Duration.create(5, "seconds"))


  override def preStart(): Unit = println("In-game actor born.")

  def receive: PartialFunction[Any, Unit] = {

    case msg: PlayerMessage =>
      var message: String = ""
      if (this.nickName.equals(msg.nick)) {
        message = "You : " + msg.message
      } else {
        message = msg.nick + ": " + msg.message
      }
      println(message)

      player ! Write(pocket141Answer(msg.charId, msg.isGm, msg.baseLvl, msg.premiumType, msg.race, message).data)
      player ! Write(pocket451Answer(message).data)


    case Received(data) => pocketNumber(data) match {
      case 125 =>
        println("125 p " + data)
      //sender() ! Write(pocket189Answer().data)


      case 137 =>
        println("137 p")
        val pData = parsePocket137(data)


      case 159 => //chat message - 141back
        println("159 here")
        val pData = parsePocket159(data)

        val senderNickname = pData.message.split(":")(0)
        val msg = pData.message.slice(senderNickname.length + 2, pData.message.length)


        if (!msg.strip().startsWith("@")) {
          MapSend ! PlayerMessage(senderNickname, msg, this.charId, this.isGM, this.baseLvl, this.premiumType, this.race)
          println(pData.message)
        }
      //sender() ! Write(pocket141Answer().data)

      case 245 =>
        val pData = parsePocket245(data)
        this.timer = pData.timer
        this.charId = pData.mapCharId
        println(pData)
        this.player = sender()
        player ! Write(pocket643Answer(pData.mapCharId).data)

        val future = SqlSend ? CordsSearch(pData.mapCharId)
        val cords = Await.result(future, timeout.duration).asInstanceOf[Cords]

        this.nickName = cords.nickName
        this.race = cords.race
        this.isGM = cords.isGm
        this.premiumType = cords.premiumType
        this.gold = cords.gold
        this.x = cords.x
        this.y = cords.y
        this.dir = cords.dir
        this.map = cords.map
        this.baseLvl = cords.baseLvl

        MapSend ! cords

        println(cords + this.map)
        player ! Write(pocket115Answer(pData.mapCharId, cords.x, cords.y, cords.dir).data)

      case 278 =>
        println("278 here")
        val pData = parsePocket278(data)
        val date = DateTime.now
        player ! Write(pocket1059Answer(date).data)

      case 1048 =>
        println("1048 here " + gold)
        player ! Write(pocketGold(gold).data)

      case 1049 =>
      //quest logic
      //sender() ! Write(pocket1025Answer())

      case 1058 =>
        println("1058 here")
        player ! Write(pocket127Answer().data)

      case 1096 => //список друзей - 1097 ответ
        println("1096 here")
        val pData = parsePocket1096(data)
        println(data)
        println(pData)

      case _ => println(pocketNumber(data))
    }


    case PeerClosed => {
      MapSend ! "dead"
      context.stop(self)
      println("In-game actor died")
      self ! PoisonPill
    }
  }

  def MapSend: ActorSelection = context.actorSelection(ActorPath("Map/" + this.map))

  def SqlSend: ActorSelection = context.actorSelection(ActorPath("Map/MapSQL"))
}

object InGamePlayer {

  case class PlayerMessage(nick: String, message: String, charId: Long, isGm: Int, baseLvl: Int, premiumType: Short, race: Short)

  case class Cords(CharacterId: Long, x: Short = 0, y: Short = 0, dir: Short = 0, gold: Long, map: String, isGm: Short, race: Short, premiumType: Short, baseLvl: Int, nickName: String)

}
