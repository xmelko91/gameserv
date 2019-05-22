package utils.sqlutils

import java.sql
import java.sql.DriverManager
import java.util.Properties

import akka.actor.Actor
import app.actors.inGame.InGamePlayer.Cords

class MapSQL extends Actor{

  import MapSQL._
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

    case CordsSearch(charId,_,_,_) =>{

      if (connection == null || connection.isClosed) connect()

      try {
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`char_account` WHERE `characterId` = ?;")
        statement.setLong(1, charId)
        println("Searrching " + charId + "   ")
        val rs = statement.executeQuery()

        if (rs.first()) {
          val x = rs.getShort("_x")
          val y = rs.getShort("_y")
          val dir = rs.getShort("_dir")
          val gold = rs.getLong("money")
          sender() ! Cords(charId, x, y, dir, gold)
        }
      }catch {
        case e => sender() ! e
      }
    }


    case _ => println
  }
}

object MapSQL{
  case class CordsSearch(CharacterId: Long, x: Short=0, y: Short=0, dir: Short=0)

}
