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
          val map = rs.getString("mapName")
          sender() ! Cords(charId, x, y, dir, gold, map)
        }
      }catch {
        case e => sender() ! e
      }
    }

    case AddNewItem(id, nameId) => {

      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`items` WHERE `NameId` = ?;")
        statement.setShort(1, nameId)
        println("Searching item " + nameId + "   ")
        val rs = statement.executeQuery()
        statement.close()
        rs.first()

        var i = 1
        val addState = connection.
          prepareStatement("INSERT INTO `" + dbName +
            "`.`player_items` (`Id`, `NameId`, `Type`, `Identified`, `TypeEquip`, `Equip`, `Attr`, `Upgrade`, `slot_1`, `slot_2`, `slot_3`, `slot_4`, `others_1`, `others_2`, `others_3`, `others_4`, `others_5`, `others_6`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
        addState.setLong(i,id)
        i+=1
        addState.setShort(i,rs.getShort("NameId"))
        i+=1
        addState.setShort(i,rs.getShort("Type"))
        i+=1
        addState.setShort(i,rs.getShort("Identified"))
        i+=1
        addState.setShort(i,rs.getShort("TypeEquip"))
        i+=1
        addState.setShort(i,rs.getShort("Equip"))
        i+=1
        addState.setShort(i,rs.getShort("Attr"))
        i+=1
        addState.setShort(i,rs.getShort("Upgrade"))
        i+=1
        addState.setShort(i,rs.getShort("slot_1"))
        i+=1
        addState.setShort(i,rs.getShort("slot_2"))
        i+=1
        addState.setShort(i,rs.getShort("slot_3"))
        i+=1
        addState.setShort(i,rs.getShort("slot_4"))
        i+=1
        addState.setLong(i,rs.getLong("others_1"))
        i+=1
        addState.setLong(i,rs.getLong("others_2"))
        i+=1
        addState.setLong(i,rs.getLong("others_3"))
        i+=1
        addState.setLong(i,rs.getLong("others_4"))
        i+=1
        addState.setLong(i,rs.getLong("others_5"))
        i+=1
        addState.setLong(i,rs.getLong("others_6"))

        addState.executeUpdate()

      }catch {
        case e => sender() ! e
      }

    }


    case _ => println
  }
}

object MapSQL{

  case class AddNewItem(charId : Long, nameId: Short)

  case class CordsSearch(CharacterId: Long, x: Short=0, y: Short=0, dir: Short=0)

}
