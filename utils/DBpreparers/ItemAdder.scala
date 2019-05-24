package utils.DBpreparers

import java.sql
import java.sql.DriverManager
import java.util.Properties

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.parsing.json.JSON

object ItemAdder extends App {

  val url: String = "jdbc:mysql://localhost/serverDB"
  val driver = "com.mysql.cj.jdbc.Driver"
  val username: String = "serge"
  val password: String = "password"
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


  val fis = Source.fromResource("items.bin").getLines()


  val arr = new ArrayBuffer[String]()
  for (i <- fis) {
    arr += i.slice(i.indexOf("\"id"), i.indexOf("}") + 1)
  }
  val items = new ArrayBuffer[Item]()

  connect()

  for (i <- arr) {
    if (i.length > 20) {
      val res = JSON.parseFull("{ " + i)
      val it = new Item()
      res match {
        case Some(e: Map[Any, Any]) => {

          for (i <- e) {
            i._1.asInstanceOf[String] match {

              case "id" => it.id = i._2.asInstanceOf[Double].intValue()
              case "weight" => it.weight = i._2.asInstanceOf[Double].intValue()
              case "name_english" => it.name_english = i._2.asInstanceOf[String]
              case "name_japanese" => it.name_japanese = i._2.asInstanceOf[String]
              case "type" => it.tyype = i._2.asInstanceOf[Double].intValue()
              case "price_buy" => it.price_buy = i._2.asInstanceOf[Double].intValue()
              case "price_sell" => it.price_sell = i._2.asInstanceOf[Double].intValue()
              case "attack" => it.attack = i._2.asInstanceOf[Double].intValue()
              case "defence" => it.defence = i._2.asInstanceOf[Double].intValue()
              case "range" => it.range = i._2.asInstanceOf[Double].intValue()
              case "slots" => it.slots = i._2.asInstanceOf[Double].intValue()
              case "equip_jobs" if i._2.getClass == classOf[String] => {
                val str = i._2.asInstanceOf[String]
                if (str.equals("0xFFFFFFFF")) it.equip_jobs = -1
                else it.equip_jobs = str.toDouble.intValue()
              }
              case "equip_jobs" if i._2.getClass == classOf[Double] => i._2.asInstanceOf[Double].intValue()
              case "equip_upper" => it.equip_upper = i._2.asInstanceOf[Double].intValue()
              case "equip_genders" => it.equip_genders = i._2.asInstanceOf[Double].intValue()
              case "equip_locations" => it.equip_locations = i._2.asInstanceOf[Double].intValue()
              case "weapon_level" => it.weapon_level = i._2.asInstanceOf[Double].intValue()
              case "equip_level" => it.equip_level = i._2.asInstanceOf[Double].intValue()
              case "refineable" => it.refineable = i._2.asInstanceOf[Double].intValue()
              case "view" => it.view = i._2.asInstanceOf[Double].intValue()
              case "durability" => it.durability = i._2.asInstanceOf[Double].intValue()
              case a@_ =>
            }
          }

        }
      }
      items += it
    }
  }


  items.foreach(i => {
    addPars(i)
    Thread.sleep(10)
  })

  def addPars(item: Item) = {
    var i = 1
    val addState = connection.
      prepareStatement("INSERT INTO `serverDB`.`items` " +
        "(`NameId`, `type`, `name_english`, `name_japanese`, `price_buy`, `price_sell`," +
        " `weight`, `attack`, `defence`, `range`, `slots`, `equip_jobs`, `equip_upper`," +
        " `equip_genders`, `equip_locations`, `weapon_level`, `equip_level`, `refineable`, `view`, `durability`)" +
        " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
    addState.setInt(i, item.id)
    i+=1
    addState.setInt(i, item.tyype)
    i+=1
    addState.setString(i, item.name_english)
    i+=1
    addState.setString(i, item.name_japanese)
    i+=1
    addState.setInt(i, item.price_buy)
    i+=1
    addState.setInt(i, item.price_sell)
    i+=1
    addState.setInt(i, item.weight)
    i+=1
    addState.setInt(i, item.attack)
    i+=1
    addState.setInt(i, item.defence)
    i+=1
    addState.setInt(i, item.range)
    i+=1
    addState.setInt(i, item.slots)
    i+=1
    addState.setInt(i, item.equip_jobs)
    i+=1
    addState.setInt(i, item.equip_upper)
    i+=1
    addState.setInt(i, item.equip_genders)
    i+=1
    addState.setInt(i, item.equip_locations)
    i+=1
    addState.setInt(i, item.weapon_level)
    i+=1
    addState.setInt(i, item.equip_level)
    i+=1
    addState.setInt(i, item.refineable)
    i+=1
    addState.setInt(i, item.view)
    i+=1
    addState.setInt(i, item.durability)

    if (addState.executeUpdate() < 0) println("fault")
  }



}

class Item() {
  var id: Int = 0
  var name_english: String = ""
  var name_japanese: String = ""
  var tyype: Int = 0
  var price_buy: Int = 0
  var price_sell: Int = 0
  var weight: Int = 0
  var attack: Int = 0
  var defence: Int = 0
  var range: Int = 0
  var slots: Int = 0
  var equip_jobs: Int = 0
  var equip_upper: Int = 0
  var equip_genders: Int = 0
  var equip_locations: Int = 0
  var weapon_level: Int = 0
  var equip_level: Int = 0
  var refineable: Int = 0
  var view: Int = 0
  var durability: Int = 0
}
