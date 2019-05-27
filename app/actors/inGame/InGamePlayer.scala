package app.actors.inGame

import akka.actor.{Actor, ActorRef, ActorSelection, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.Timeout
import akka.http.scaladsl.model.DateTime
import app.Settings.ActorPath
import utils.answers.InGameAnswer
import utils.parsing.InGameParse
import akka.pattern.ask
import app.actors.inGame.InGamePlayer.{CalculatedStats, CharBaseStats, CharacterCoordinates, Cords, PlayerMessage, UpStat}
import app.actors.inGame.MapInstance.NPC
import utils.sqlutils.MapSQL.{CordsSearch, GetAllItems, ItemsSet}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class InGamePlayer extends Actor with InGameParse with InGameAnswer {


  var stats: CharBaseStats = _
  var player: ActorRef = _
  var nickName: String = ""
  var race: Short = 0
  var sex: Short = 0
  var premiumType: Short = 0
  var hairColor: Short = 0
  var baseLvl: Int = 0
  var jobId: Short = 0
  var isGM: Short = 0
  var charId: Long = 0
  var gold: Long = 0
  var timer: Long = 0
  var PC: CharacterCoordinates = _
  var map: String = ""
  var playerItems: Array[ItemsSet] = _
  var NPCs: Array[(NPC, Boolean)] = _
  implicit val timeout: Timeout = Timeout(Duration.create(5, "seconds"))


  override def preStart(): Unit = println("In-game actor born.")

  def receive: PartialFunction[Any, Unit] = {

    case msg: PlayerMessage =>
      var pon: Short = 1
      if (this.race == msg.race || msg.premiumType == 2 || this.premiumType == 1 || this.premiumType == 2) pon = 0
      player ! Write(pocket141Answer(msg.charId, msg.isGm, msg.baseLvl, msg.premiumType, pon, msg.message, msg.messageLength).data)

    case a:Array[(NPC, Boolean)] => {
      NPCs = a
    }


    case Received(data) => pocketNumber(data) match {
      case 125 =>
        val s: CalculatedStats = StatsCalculating(stats, playerItems)
        println("125 p " + data)
        player ! Write(pocket189Answer(s).data)
        NPCs.foreach(p => player ! Write(pocket120Answer(p._1).data))


        player ! Write(pocket554Answer(this.charId,3,0,0,0,this.jobId,1,
          0,0,0,this.hairColor,this.race,0,0,0,0,this.sex,
          encodeCord(this.PC.X), encodeCord(this.PC.Y), encodeDir(this.PC.DIR), this.isGM, 0,1).data)

        player ! Write(pocket494Answer(playerItems).data)
        player ! Write(pocket164Answer(playerItems).data)



      case 137 =>
        println("137 p")
        val pData = parsePocket137(data)
        println(data)
        val ar = GetEncoded3(Array(pData.x, pData.y, pData.dir))


        player ! Write(pocket556Answer(charId, 1, 0, 0, 0, this.jobId,
          0, 1, 1, 0, this.hairColor, this.race, 0, 0, 1, 0, this.sex,
          this.PC.X1, this.PC.Y1, ar.x, ar.y, 0, 0, this.baseLvl).data)
        this.PC.X1 = pData.x
        this.PC.Y1 = pData.y
        this.PC.DIR1 = pData.dir

      case 155 =>
        sender() ! Write(pocket149Answer(this.nickName, this.charId).data)


      case 159 => //chat message - 141back
        val pData = parsePocket159(data)

        val senderNickname = pData.message.split(":")(0)
        val msg = pData.message.slice(senderNickname.length + 2, pData.message.length)


        if (!msg.strip().startsWith("@")) {
          MapSend ! PlayerMessage(pData.message, pData.message_count, this.charId, this.isGM, this.baseLvl, this.premiumType, this.race)
          println(pData.message)
        }
      //sender() ! Write(pocket141Answer().data)

      case 178 => //свич перса

        sender() ! Write(pocket128Answer(this.charId).data)
        sender() ! Write(pocket179Answer().data)

        MapSend ! "dead"
        context.stop(self)
        println("In-game actor died")
        self ! PoisonPill

      case 187 =>
        val pData = parsePocket187(data)
        println(pData)
        def refreshStats(skillCount: Int) = Write(pocket176Answer(9, skillCount).data)
        try {
          pData.stat match {
            case 13 => //str
              val fut = SqlSend ? UpStat(this.charId, "strrr")
              val res = Await.result(fut, timeout.duration).asInstanceOf[(Short, Short)]
              if (res._1 == (-1).shortValue()) {
                player ! Write(pocket188Answer(0, 0, 0).data)
              } // отправляем ошибку
              else {
                this.stats.str = res._1
                this.stats.skillCount = res._2
                player ! Write(pocket188Answer(1, 13, this.stats.str).data)
                player ! refreshStats(this.stats.skillCount)
                //отправляем паккет ОК
              }
            case 14 => //agi
              val fut = SqlSend ? UpStat(this.charId, "agiii")
              val res = Await.result(fut, timeout.duration).asInstanceOf[(Short, Short)]
              if (res._1 == (-1).shortValue()) {
                player ! Write(pocket188Answer(0, 0, 0).data)
              } // отправляем ошибку
              else {
                this.stats.agi = res._1
                this.stats.skillCount = res._2
                player ! Write(pocket188Answer(1, 14, this.stats.agi).data)
                player ! refreshStats(this.stats.skillCount)
                //отправляем паккет ОК
              }
            case 15 => //vit
              val fut = SqlSend ? UpStat(this.charId, "vittt")
              val res = Await.result(fut, timeout.duration).asInstanceOf[(Short, Short)]
              if (res._1 == (-1).shortValue()) {
                player ! Write(pocket188Answer(0, 0, 0).data)
              } // отправляем ошибку
              else {
                this.stats.vit = res._1
                this.stats.skillCount = res._2
                player ! Write(pocket188Answer(1, 15, this.stats.vit).data)
                player ! refreshStats(this.stats.skillCount)
                //отправляем паккет ОК
              }
            case 16 => //int
              val fut = SqlSend ? UpStat(this.charId, "inttt")
              val res = Await.result(fut, timeout.duration).asInstanceOf[(Short, Short)]
              if (res._1 == (-1).shortValue()) {
                player ! Write(pocket188Answer(0, 0, 0).data)
              } // отправляем ошибку
              else {
                this.stats.int = res._1
                this.stats.skillCount = res._2
                player ! Write(pocket188Answer(1, 16, this.stats.int).data)
                player ! refreshStats(this.stats.skillCount)
                //отправляем паккет ОК
              }
            case 17 => //dex
              val fut = SqlSend ? UpStat(this.charId, "dexxx")
              val res = Await.result(fut, timeout.duration).asInstanceOf[(Short, Short)]
              if (res._1 == (-1).shortValue()) {
                player ! Write(pocket188Answer(0, 0, 0).data)
              } // отправляем ошибку
              else {
                this.stats.dex = res._1
                this.stats.skillCount = res._2
                player ! Write(pocket188Answer(1, 17, this.stats.dex).data)
                player ! refreshStats(this.stats.skillCount)
                //отправляем паккет ОК
              }
            case 18 => //luk
              val fut = SqlSend ? UpStat(this.charId, "lukkk")
              val res = Await.result(fut, timeout.duration).asInstanceOf[(Short, Short)]
              if (res._1 == (-1).shortValue()) {
                player ! Write(pocket188Answer(0, 0, 0).data)
              } // отправляем ошибку
              else {
                this.stats.luk = res._1
                this.stats.skillCount = res._2
                player ! Write(pocket188Answer(1, 18, this.stats.luk).data)
                player ! refreshStats(this.stats.skillCount)
                //отправляем паккет ОК
              }
          }
        } catch {
          case _:Throwable => player ! Write(pocket188Answer(0, 0, 0).data)
        }

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
        this.PC = new CharacterCoordinates(cords.x, cords.y, cords.dir, cords.x1, cords.y1, cords.dir1)
        this.map = cords.map
        this.baseLvl = cords.baseLvl
        this.jobId = cords.jobId
        this.sex = cords.sex
        this.stats = cords.stats
        this.hairColor = cords.hairColor

        val getItems = SqlSend ? GetAllItems(this.charId)
        this.playerItems = Await.result(getItems, timeout.duration).asInstanceOf[Array[ItemsSet]]

        MapSend ! cords

        println(cords + this.map)
        player ! Write(pocket115Answer(pData.mapCharId, cords.x, cords.y, cords.dir).data)

      case 278 =>
        //val pData = parsePocket278(data)
        val date = DateTime.now
        player ! Write(pocket1059Answer(date).data)

      case 1048 =>
        println("1048 here " + gold)
        player ! Write(pocketGold(gold, 0).data)

      case 1049 =>
      //quest logic
      //sender() ! Write(pocket1025Answer())

      case 1058 =>
        player ! Write(pocket127Answer().data)

      case 1096 => //список друзей - 1097 ответ
        val pData = parsePocket1096(data)


      case _ => println(pocketNumber(data))
    }


    case PeerClosed =>
      MapSend ! "dead"
      context.stop(self)
      println("In-game actor died")
      self ! PoisonPill
  }

  def MapSend: ActorSelection = context.actorSelection(ActorPath("Map/" + this.map))

  def SqlSend: ActorSelection = context.actorSelection(ActorPath("Map/MapSQL"))
}

object InGamePlayer {

  case class UpStat(charId: Long, stat: String)

  case class CalculatedStats(Stats_count: Int, _str: Short, _str_append: Short,
                             _agi: Short, _agi_append: Short, _vit: Short,
                             _vit_append: Short, _int: Short, _int_append: Short,
                             _dex: Short, _dex_append: Short, _luk: Short,
                             _luk_append: Short, atk1: Int, atk2: Int,
                             matkMax: Int, matkMin: Int, def1: Int,
                             def2: Int, mdef1: Int, mdef2: Int,
                             hit: Int, flee1: Int, flee2: Int,
                             critical: Int, karma: Int, manner: Int)

  class CharBaseStats(st: Short, ag: Short, vi: Short, in: Short, de: Short, lu: Short, skillCoun: Short, statCou: Short) {
    var str: Short = st
    var agi: Short = ag
    var vit: Short = vi
    var int: Short = in
    var dex: Short = de
    var luk: Short = lu
    var skillCount: Short = skillCoun
    var statCout: Short = statCou
  }

  class CharacterCoordinates(x:Short, y:Short, dir: Short, x1:Short, y1:Short, dir1:Short){
    var X: Short = x
    var Y: Short = y
    var DIR: Short = dir
    var X1: Short = x1
    var Y1: Short = y1
    var DIR1: Short = dir1
  }

  case class PlayerMessage(message: String, messageLength: Int, charId: Long, isGm: Int, baseLvl: Int, premiumType: Short, race: Short)

  case class Cords(CharacterId: Long, x: Short = 0, x1: Short, y: Short = 0, y1: Short, dir: Short = 0, dir1:Short,
                   gold: Long, map: String, isGm: Short, race: Short, premiumType: Short, baseLvl: Int,
                   nickName: String, jobId: Short, sex: Short, hairColor: Short, stats: CharBaseStats)

}
