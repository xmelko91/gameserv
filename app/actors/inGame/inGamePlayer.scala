package app.actors.inGame

import akka.actor.{Actor, ActorSelection, PoisonPill}
import akka.io.Tcp.{PeerClosed, Received, Write}
import akka.util.{ByteString, Timeout}
import akka.actor.Actor
import utils.parsing.InGameParse

class inGamePlayer extends Actor with InGameParse{

  def receive: PartialFunction[Any, Unit] = {
    case Received(data) => println(pocketNumber(data))


    case PeerClosed => {
      context.stop(self)
      println("Player actor died")
      self ! PoisonPill
    }
  }
}
