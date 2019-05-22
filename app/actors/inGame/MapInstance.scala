package app.actors.inGame

import akka.actor.{Actor, ActorRef}
import app.actors.inGame.InGamePlayer.Cords

import scala.collection.mutable.ArrayBuffer

class MapInstance extends Actor {

  import MapInstance._

  val playersOnMap = new ArrayBuffer[Player]()

  override def receive: Receive = {
    case Cords(id, x, y, dir,_,_) =>
      println("Player on map")
      playersOnMap += Player(sender(), x, y, dir, id)

    case a@_ => println(a)
  }
}

object MapInstance{
  case class Player(ref : ActorRef, x: Int, y: Int, dir: Int, id: Long)
}
