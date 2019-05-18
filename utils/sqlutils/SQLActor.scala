package utils.sqlutils

import java.sql
import java.sql.{Date, DriverManager}
import java.util.Properties

import akka.actor.Actor
import app.actors.LoginActor.{CharacterInfo, CheckUserInDB, UserInfo}
import utils.parsing.DataFunc


class SQLActor extends Actor with DataFunc{

  import SQLActor._
  import app.Settings._

  val url: String = "jdbc:mysql://localhost/" + dbName
  val driver = "com.mysql.cj.jdbc.Driver"
  val username: String = nmSQL
  val password: String = pswSQL
  var connection: sql.Connection = _

  def connect(): Unit = {
    Class.forName(driver)
    val p = new Properties()
    p.setProperty("useSSL", "false")
    p.setProperty("serverTimezone", "UTC")
    p.setProperty("user", username)
    p.setProperty("password",password)
    p.setProperty("useUnicode","true")
    p.setProperty("characterEncoding","cp1251")
    connection = DriverManager.getConnection(url, p)
  }

  override def receive: Receive = {


    case SearchProps(stat, column, table) =>
      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.createStatement()
        val n = "SELECT * FROM " + dbName + "." + table + " WHERE " + column + " = '"+ stat +"';"
        val rs = statement.executeQuery(n)
        rs.first()
        var id = ""
        id = rs.getString(column)
        println(id + " sended")
        sender() ! id
      } catch {
        case e => sender() ! ""
      }

    case AddNewCharacter(info, cInfo) =>
      if (connection == null || connection.isClosed) connect()
      try {
        println("adding")
        var ID: Long = -1

        var i:Int=1
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`login_account` WHERE `login` = ? AND `pass` = ?;")
        statement.setString(i, info.login)
        i+=1
        statement.setString(i, info.password)
        val rs = statement.executeQuery()
        rs.next()
        ID = rs.getLong("loginAccountId")
        println("ID got " + ID)

        if (ID != -1) {
          i = 1
          val statementUpdate = connection.prepareStatement("INSERT INTO `" + dbName +
            "`.`char_account` (`loginAccountId`, `jobId`, `_local_3`, `clothesColor`, `hairColor`, `name`, `slot`) VALUES ('"+
            ID + "', '"+cInfo.jobId + "', '" + cInfo.local3 + "', '"+ cInfo.clothesColor +"', '"+ cInfo.hairColor +"', '"+ cInfo.name +"', '"+ cInfo.slot +"');")
          statementUpdate.executeUpdate()
          statementUpdate.close()

          val statement = connection.createStatement()
          val n = "SELECT * FROM " + dbName + ".char_account WHERE `name` = '"+ cInfo.name +"';"
          val rs = statement.executeQuery(n)
          rs.first()
          var charId:Long = -1
          charId = rs.getInt("characterId")
          println("Adding is : " + charId)
          sender() ! (charId, ID)
        }
        else sender() ! (-1, -1)
      } catch {
        case e => {
          e.printStackTrace()
          sender() ! (-1, -1)
        }
      }

    case AddNewPlayer(info) => {
      if (connection == null || connection.isClosed) connect()
      try {
        println("adding character")
        var rs = -1
        val statement = connection.createStatement()
        rs = statement.executeUpdate("INSERT INTO `" + dbName + "`.`login_account` (`login`, `pass`) VALUES ('" + info.login + "', '" + info.password + "');")
        sender() ! rs
      } catch {
        case e => sender() ! -1
      }
    }

    case CheckUserInDB(login, pass) => {
      println(login + "    " + pass)
      if (connection == null || connection.isClosed) connect()
      try {
        var i: Int = 1
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`login_account` WHERE login = ? AND pass = ?;")
        statement.setString(i, login)
        i = i + 1
        statement.setString(i, pass)
        val rs = statement.executeQuery()

        var loginId1: Long = -1
        rs.next()

        val loginAccId = rs.getLong("loginAccountId")
        loginId1 = loginAccId
        val loginId2 = loginAccId
        val numSlots = rs.getShort("numSlots")
        val premiumType = rs.getShort("premiumType")
        val premiumUntil = rs.getLong("premiumUntil")
        val banned = rs.getDate("Baned")

        sender() ! PlayerLoginStats(loginId1, loginAccId, loginId2, numSlots, premiumType, premiumUntil, banned)

      } catch {
        case e => sender() ! PlayerLoginStats(-1, 0, 0, 0, 0, 0, null)
      }
    }
  }

  override def postStop(): Unit = println("SQL Actor died.")
}

object SQLActor {

  case class AddNewCharacter(info: UserInfo, cInfo: CharacterInfo)

  case class AddNewPlayer(info: UserInfo)

  case class PlayerLoginStats(loginId1: Long, loginAccId: Long, loginId2: Long, numSlots: Short, premiumType: Short, premiumUntil: Long, banned: Date)

  case class SearchProps(id: String, param: String, table: String)

  case class InsertProps()


}
