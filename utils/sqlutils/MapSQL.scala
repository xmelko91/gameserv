package utils.sqlutils

import java.sql
import java.sql.DriverManager
import java.util.Properties

import akka.actor.Actor
import app.actors.inGame.InGamePlayer._
import app.actors.inGame.MapInstance.{MapNpc, NPC}
import utils.parsing.MathUtils

import scala.collection.mutable.ArrayBuffer

class MapSQL extends Actor with MathUtils{

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

    case CordsSearch(charId, _, _, _) => {

      if (connection == null || connection.isClosed) connect()

      try {
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`char_account` WHERE `characterId` = ?;")
        statement.setLong(1, charId)
        println("Searrching " + charId + "   ")
        val rs = statement.executeQuery()

        if (rs.first()) {
          val x = rs.getShort("_x")
          val y = rs.getShort("_y")
          val x1 = rs.getShort("_x1")
          val y1 = rs.getShort("_y1")
          val dir = rs.getShort("_dir")
          val dir1 = rs.getShort("_dir1")
          val gold = rs.getLong("money")
          val map = rs.getString("mapName")
          val race = rs.getShort("clothesColor")
          val userId = rs.getLong("loginAccountId")
          val baseLvl = rs.getInt("baseLevel")
          val nick = rs.getString("name")
          val jobId = rs.getShort("jobId")
          val sex = rs.getShort("_local_3")
          val hairColor = rs.getShort("hairColor")
          val str = rs.getShort("strrr")
          val agi = rs.getShort("agiii")
          val vit = rs.getShort("vittt")
          val int = rs.getShort("inttt")
          val dex = rs.getShort("dexxx")
          val luk = rs.getShort("lukkk")
          val statCount = rs.getShort("stats_count")
          val skillCount = rs.getShort("skill_count")
          val stats: CharBaseStats = new CharBaseStats(str,agi,vit,int,dex,luk,skillCount, statCount)

          println("ID got " + userId)
          val stateUser = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`login_account` WHERE `loginAccountId` = ?;")
          stateUser.setLong(1, userId)
          val uRs = stateUser.executeQuery()

          if (uRs.first()) {

            val isGm = uRs.getShort("isGM")
            val premiumType = uRs.getShort("premiumType")


            sender() ! Cords(charId, x, x1, y, y1, dir, dir1, gold, map, isGm, race, premiumType, baseLvl, nick, jobId, sex, hairColor, stats)
          }
        }

      } catch {
        case e:Throwable => {
          e.printStackTrace()
          sender() ! e
        }
      }
    }

    case AddNewItem(id, nameId, amount) => {

      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`items` WHERE `NameId` = ?;")
        statement.setShort(1, nameId)
        println("Searching item " + nameId + "   ")
        val rs = statement.executeQuery()
        rs.first()

        println("adding new Item")
        var i = 1
        val addState = connection.
          prepareStatement("INSERT INTO `" + dbName +
            "`.`player_items` (`Id`, `NameId`, `Type`, `amount`) VALUES (?, ?, ?, ?);")

        addState.setLong(i, id)
        i += 1
        addState.setShort(i, rs.getShort("NameId"))
        i += 1
        addState.setShort(i, rs.getShort("type"))
        i += 1
        addState.setInt(i, amount)

        addState.executeUpdate()

      } catch {
        case e => sender() ! e
      }
    }

    case UpStat(id, stat) =>{
      if (connection == null || connection.isClosed) connect()

      try {
        val statement = connection.prepareStatement("SELECT "+ stat +", stats_count FROM `" + dbName + "`.`char_account` WHERE `characterId` = ?;")
        statement.setLong(1, id)
        val rs = statement.executeQuery()
        if (rs.first()) {
          println(id)
          val par = rs.getShort(stat)
          val statsCount = rs.getShort("stats_count")
          if (statsCount < upgradePrice(par)) sender() ! (-1, -1)
          else {
            val state = connection.prepareStatement("UPDATE `" + dbName + "`.`char_account` SET `" + stat + "` = ?, `stats_count` = ? WHERE (`characterId` = ?);")
            state.setShort(1, (par + 1).shortValue())
            state.setShort(2, (statsCount - upgradePrice(par)).shortValue())
            state.setLong(3, id)
            state.executeUpdate()
          }
          sender() ! ((par + 1).shortValue(), (statsCount - upgradePrice(par)).shortValue())
        }else sender() ! ((-1).shortValue(), (-1).shortValue())
      }catch {
        case e:Throwable => {
          sender() ! e
        }
      }
    }

      //@param return Array[ItemsSet]
    case GetAllItems(id) => {
      val out = new ArrayBuffer[ItemsSet]()
      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`player_items` WHERE `Id` = ?;")
        statement.setLong(1, id)
        val rs = statement.executeQuery()
        while (rs.next()){
          val nameId = rs.getInt("NameId")
          val tyype = rs.getShort("Type")
          val identified = rs.getShort("identified")
          val typeEquip = rs.getShort("TypeEquip")
          val equip = rs.getShort("Equip")
          val attr = rs.getShort("Attr")
          val upgrade = rs.getShort("Upgrade")
          val slot1 = rs.getShort("slot_1")
          val slot2 = rs.getShort("slot_2")
          val slot3 = rs.getShort("slot_3")
          val slot4 = rs.getShort("slot_4")
          val others1 = rs.getLong("others_1")
          val others2 = rs.getLong("others_2")
          val others3 = rs.getLong("others_3")
          val others4 = rs.getLong("others_4")
          val others5 = rs.getLong("others_5")
          val others6 = rs.getLong("others_6")
          val amount = rs.getInt("amount")

          val pItem = CharItem(id, nameId, tyype,
            identified, typeEquip, equip, attr,
            upgrade, slot1, slot2, slot3, slot4,
            others1, others2, others3, others4, others5, others6, amount)

          val iState = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`items` WHERE `NameId` = ?;")
          iState.setShort(1, nameId.shortValue())
          val result = iState.executeQuery()
          if (result.first()){
            val iType = result.getInt("type")
            val nameEnglish = result.getString("name_english")
            val nameJap = result.getString("name_japanese")
            val priceBuy = result.getInt("price_buy")
            val priceSell = result.getInt("price_sell")
            val weight = result.getInt("weight")
            val attack = result.getInt("attack")
            val defence = result.getInt("defence")
            val range = result.getInt("range")
            val slots = result.getInt("slots")
            val equipJobs = result.getInt("equip_jobs")
            val equipUpper = result.getInt("equip_upper")
            val equipGenders = result.getInt("equip_genders")
            val equipLocations = result.getInt("equip_locations")
            val weaponLevel = result.getInt("weapon_level")
            val equipLevel = result.getInt("equip_level")
            val refineable = result.getInt("refineable")
            val view = result.getInt("view")
            val durability = result.getInt("durability")

            val item = ItemParams(nameId, nameEnglish, nameJap,
              iType, priceBuy, priceSell, weight, attack, defence,
              range, slots,equipJobs, equipUpper, equipGenders,
              equipLocations, weaponLevel, equipLevel, refineable,
              view, durability)

            out += ItemsSet(pItem, item)
          }
        }
        //возвращает Array из предметов игрока
        sender() ! out.toArray

      } catch {
        case e:Throwable => sender() ! out.toArray
      }
    }

    case MapNpc(name) =>
      val out = new ArrayBuffer[(NPC, Boolean)]()
      if (connection == null || connection.isClosed) connect()
      try {
        val statement = connection.prepareStatement("SELECT * FROM `" + dbName + "`.`npc` WHERE `MapName` = ?;")
        statement.setString(1, name)
        val rs = statement.executeQuery()
        while (rs.next()){
          val id = rs.getLong("Id")
          val walkSpeed = rs.getShort("walkSpeed")
          val l4 = rs.getShort("_local_4")
          val l5 = rs.getShort("_local_5")
          val l6 = rs.getShort("_local_6")
          val jobId = rs.getShort("jobId")
          val sex = rs.getShort("sex")
          val x = rs.getShort("x")
          val y = rs.getShort("y")
          val dir = rs.getShort("dir")
          val isDead = rs.getShort("isDead")
          val baseLevel = rs.getShort("baseLevel")
          val scriptName = rs.getString("ScriptName")
          val name = rs.getString("Name")
          val icon = rs.getShort("Icon")
          val chIndex = rs.getShort("CharacterIndex")
          val mapName = name
          val desc = rs.getString("Description")
          val mMap = rs.getInt("MainMap")
          val tyype = rs.getInt("Type")
          val npc = (NPC(id,walkSpeed,l4,l5,l6,jobId,sex,x,y,dir,isDead,baseLevel,scriptName,name,icon,chIndex,mapName,desc,mMap, tyype), false)
          out += npc
        }

        sender() ! out.toArray
      }catch {
        case e:Throwable => sender() ! out.toArray
      }

    case _ => println
  }
}

object MapSQL {

  case class ItemsSet(item: CharItem, iemParams: ItemParams)

  case class GetAllItems(charId: Long)

  case class AddNewItem(charId: Long, nameId: Short, amount: Int = 1)

  case class CordsSearch(CharacterId: Long, x: Short = 0, y: Short = 0, dir: Short = 0)

  case class CharItem(
                       id : Long,
                       nameId: Int,
                       tyype: Short,
                       identified: Short,
                       typeEquip: Short,
                       equip: Short,
                       attr: Short,
                       upgrade: Short,
                       slot1: Short,
                       slot2: Short,
                       slot3: Short,
                       slot4: Short,
                       others1: Long,
                       others2: Long,
                       others3: Long,
                       others4: Long,
                       others5: Long,
                       others6: Long,
                       amount: Int
                     )

  case class ItemParams(
                         id: Int,
                         name_english: String,
                         name_japanese: String,
                         tyype: Int,
                         price_buy: Int,
                         price_sell: Int,
                         weight: Int,
                         attack: Int,
                         defence: Int,
                         range: Int,
                         slots: Int,
                         equip_jobs: Int,
                         equip_upper: Int,
                         equip_genders: Int,
                         equip_locations: Int,
                         weapon_level: Int,
                         equip_level: Int,
                         refineable: Int,
                         view: Int,
                         durability: Int
                       )

}
