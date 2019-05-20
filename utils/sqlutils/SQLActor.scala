package utils.sqlutils

import java.sql
import java.sql.{Date, DriverManager}
import java.util.Properties

import akka.actor.Actor
import app.actors.preStartGame.LoginActor.{CharacterInfo, CheckUserInDB, UserInfo}
import utils.parsing.DataFunc

import scala.collection.mutable.ArrayBuffer


class SQLActor extends Actor with DataFunc {

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
    p.setProperty("password", password)
    p.setProperty("useUnicode", "true")
    p.setProperty("characterEncoding", "cp1251")
    connection = DriverManager.getConnection(url, p)
  }

  override def receive: Receive = {


    case SearchProps(stat, column, table) => {
      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.createStatement()
        val n = "SELECT * FROM " + dbName + "." + table + " WHERE " + column + " = '" + stat + "';"
        val rs = statement.executeQuery(n)
        if (rs.first()) {
          val id = rs.getString(column)
          println(id + " sended")
          sender() ! id
        }else sender() ! ""
      } catch {
        case e => {
          e.printStackTrace()
        }
      }
    }

    case AddNewCharacter(info, cInfo, logId) => {
      if (connection == null || connection.isClosed) connect()
      try {
        println("adding")
        var ID: Long = -1
        var i: Int = 1


        if (!info.login.equals("1")) {
          val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`login_account` WHERE `login` = ? AND `pass` = ?;")
          statement.setString(i, info.login)
          i += 1
          statement.setString(i, info.password)
          val rs = statement.executeQuery()
          rs.next()
          ID = rs.getLong("loginAccountId")
          println("ID got " + ID)
        }else{
          ID = logId
        }

        if (ID != -1) {
          i = 1
          val statementUpdate = connection.prepareStatement("INSERT INTO `" + dbName +
            "`.`char_account` (`loginAccountId`, `jobId`, `_local_3`, `clothesColor`, `hairColor`, `name`, `slot`) VALUES ('" +
            ID + "', '" + cInfo.jobId + "', '" + cInfo.local3 + "', '" + cInfo.clothesColor + "', '" + cInfo.hairColor + "', '" + cInfo.name + "', '" + cInfo.slot + "');")
          statementUpdate.executeUpdate()
          statementUpdate.close()

          val statement = connection.createStatement()
          val n = "SELECT * FROM " + dbName + ".char_account WHERE `name` = '" + cInfo.name + "';"
          val rs = statement.executeQuery(n)
          rs.first()
          var charId: Long = -1
          charId = rs.getInt("characterId")
          println("Adding is : " + charId)
          sender() ! (charId, ID)
        }
        else sender() ! (1.longValue(), 1.longValue())
      } catch {
        case e => {
          e.printStackTrace()
          sender() ! (-1.longValue(), -1.longValue())
        }
      }
    }

    case AddNewPlayer(info) => {
      if (connection == null || connection.isClosed) connect()
      try {
        if (!info.login.equals("1")) {
          println("adding character")
          var rs = -1
          val statement = connection.createStatement()
          rs = statement.executeUpdate("INSERT INTO `" + dbName + "`.`login_account` (`login`, `pass`) VALUES ('" + info.login + "', '" + info.password + "');")
          sender() ! rs.longValue()
        }else{
          sender() ! 0
        }
      } catch {
        case e => {
          e.printStackTrace()
          sender() ! -1
        }
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

        if (rs.first()) {

          val loginAccId = rs.getLong("loginAccountId")
          loginId1 = loginAccId
          val loginId2 = loginAccId
          val numSlots = rs.getShort("numSlots")
          val premiumType = rs.getShort("premiumType")
          val premiumUntil = rs.getLong("premiumUntil")
          val banned = rs.getDate("Baned")

          sender() ! PlayerLoginStats(loginId1, loginAccId, loginId2, numSlots, premiumType, premiumUntil, banned)
        }else{
          sender() ! PlayerLoginStats(-1, 0, 0, 0, 0, 0, null)
        }
      } catch {
        case e => e.printStackTrace()
      }
    }

    case AllChars(loginId, slot) => {
      import utils.answers.CharacterAnswer._

      if (connection == null || connection.isClosed) connect()
      try {
        if (slot < 0) {
          val out = new ArrayBuffer[CharStats]()
          val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`char_account` WHERE loginAccountId = ?;")

          statement.setLong(1, loginId)
          val rs = statement.executeQuery()
          while (rs.next()) {

            val charId = rs.getLong("characterId")
            val baseExp = rs.getLong("baseExp")
            val money = rs.getLong("money")
            val jobExp = rs.getLong("jobExp")
            val jobLvl = rs.getInt("jobLevel")
            val hp = rs.getInt("hp")
            val maxHp = rs.getInt("maxHp")
            val sp = rs.getShort("sp")
            val maxSp = rs.getInt("maxSp")
            val jobId = rs.getInt("jobId")
            val local3 = rs.getInt("_local_3")
            val baseLvl = rs.getInt("baseLevel")
            val hairColor = rs.getInt("hairColor")
            val clothesColor = rs.getInt("clothesColor")
            val name = rs.getString("name")
            val str = rs.getShort("strrr")
            val agi = rs.getShort("agiii")
            val vit = rs.getShort("vittt")
            val int = rs.getShort("inttt")
            val dex = rs.getShort("dexxx")
            val luk = rs.getShort("lukkk")
            val slot = rs.getInt("slot")
            val renames = rs.getInt("renames")

            val char = CharStats(
              charId, baseExp, money, jobExp, jobLvl,
              hp, maxHp, sp, maxSp, jobId, local3,
              baseLvl, hairColor, clothesColor, name
              , str, agi, vit, int, dex, luk
              , slot, renames)

            out += char
          }

          sender() ! out.toArray
        }
        else {
          val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`char_account` WHERE loginAccountId = ? AND slot = ?;")

          statement.setLong(1, loginId)
          statement.setInt(2, slot)
          val rs = statement.executeQuery()
          if (rs.first()){
            val charId = rs.getLong("characterId")

            sender() ! charId
          }
        }

      }catch {
        case e => {
          e.printStackTrace()
          sender() ! null
        }
      }

    }

    case ChangeNickName(id, nick) => {
      try {
        println("setting name " + nick)
        val str = "UPDATE `serverDB`.`char_account` SET `name` = '"+ nick +"', `renames` = '1' WHERE (`characterId` = '"+ id.toString +"');"
        val statement = connection.prepareStatement(str)
        val rs = statement.executeUpdate()
        sender() ! rs
      }catch {
        case e =>
          e.printStackTrace()
          sender() ! -1
      }
    }

  }

  override def postStop(): Unit = println("SQL Actor died.")
}

object SQLActor {

  case class ChangeNickName(id: Long, name: String)

  case class AddNewCharacter(info: UserInfo, cInfo: CharacterInfo, logId: Long)

  case class AddNewPlayer(info: UserInfo)

  case class PlayerLoginStats(loginId1: Long, loginAccId: Long, loginId2: Long, numSlots: Short, premiumType: Short, premiumUntil: Long, banned: Date)

  case class SearchProps(id: String, param: String, table: String)

  case class AllChars(loginId: Long, slot: Int = -1)

  case class InsertProps()


}
