package utils.sqlutils

import java.sql
import java.sql.DriverManager

import akka.actor.Actor
import app.actors.LoginActor.CheckUserInDB

import scala.util.control.Breaks

class SQLActor extends Actor{
  import SQLActor._

  val url = "jdbc:mysql://localhost/serverDB"
  val driver = "com.mysql.jdbc.Driver"
  val username = "serge"
  val password = "password"
  var connection:sql.Connection = _

  def connect(): Unit = {
    Class.forName(driver)
    connection = DriverManager.getConnection(url, username, password)
  }

  override def receive: Receive = {
    case SearchProps(playerId : Int, param:String, table:String) =>
      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.createStatement()
        val rs = statement.executeQuery("SELECT * FROM "+table+" WHERE " + param + " = " + playerId + 1 + ";")
        var id:Int = -1
        rs.next()
        id = rs.getInt("idPlayer")
        sender() ! id
      }catch {
        case e => sender() ! -1
      }



    case CheckUserInDB(login, pass) =>
      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.createStatement()
        val rs = statement.executeQuery("SELECT * FROM login_account WHERE login = '" + login + "' AND password = '" + pass + "';")

        var loginId1:Int = -1
        rs.next()

        loginId1 = rs.getInt("loginId1")
        val loginId2 = rs.getInt("loginId2")
        val loginAccId = rs.getInt("loginAccountId")
        val numSlots = rs.getByte("numSlots")
        val premiumType = rs.getByte("premiumType")
        val premiumUntil = rs.getInt("premiumUntil")

        sender() ! PlayerLoginStats(loginId1, loginAccId, loginId2, numSlots, premiumType, premiumUntil)

      }catch {
        case e => sender() ! PlayerLoginStats(-1,0,0,0,0,0)
      }
  }
}

object SQLActor{
  case class PlayerLoginStats(loginId1: Int, loginAccId: Int, loginId2: Int, numSlots: Byte, premiumType: Byte, premiumUntil: Int)
  case class SearchProps(id:Any, param:String, table:String)
  case class InsertProps()
}
