package app.actors.inGame

import akka.actor.{Actor, ActorRef}
import app.Settings.ActorPath
import akka.pattern.ask
import akka.util.Timeout
import app.actors.inGame.InGamePlayer.{Cords, PlayerMessage}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class MapInstance extends Actor {

  import MapInstance._

  val playersOnMap = new ArrayBuffer[Player]()
  var NPCarr: Array[(NPC, Boolean)] = _
  implicit val timeout: Timeout = Timeout(Duration.create(5, "seconds"))

  override def preStart(): Unit = {
    val answ = context.actorSelection(ActorPath("Map/MapSQL")) ? MapNpc(context.self.path.name)
    NPCarr = Await.result(answ, timeout.duration).asInstanceOf[Array[(NPC, Boolean)]]
  }

  override def receive: Receive = {
    case c: Cords =>
      println("Player on map")
      playersOnMap += Player(sender(), c)
      sender() ! NPCarr

    case msg : PlayerMessage =>
      //можно добавить проверку мата и тп здесь
      for (p <- playersOnMap){
        p.ref ! msg
      }

    case "dead" =>
      playersOnMap.map(x => {
        if (x.ref.equals(sender())){
          playersOnMap -= x
        }
      })
    case a@_ => println(a)
  }
}

object MapInstance{
  case class MapNpc(name: String)
  case class Player(ref : ActorRef, stats: Cords)
  case class NPC(Id: Long, walkSpeed: Short, local4: Short, local5:Short, local6: Short,
                 jobId: Short, sex:Short, x:Short, y:Short, dir: Short, isDead:Short,
                 baseLvl:Short, scriptName: String, name: String, icon: Short,
                 charIndex: Short, mapName : String, description: String, MainMap: Int, Type: Int)
}
