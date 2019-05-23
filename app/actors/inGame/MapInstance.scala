package app.actors.inGame

import akka.actor.{Actor, ActorRef}
import app.actors.inGame.InGamePlayer.{Cords, PlayerMessage}

import scala.collection.mutable.ArrayBuffer

class MapInstance extends Actor {

  import MapInstance._

  val playersOnMap = new ArrayBuffer[Player]()

  override def receive: Receive = {
    case c: Cords =>
      println("Player on map")
      playersOnMap += Player(sender(), c)

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
  case class Player(ref : ActorRef, stats: Cords)
}
