package utils.DBpreparers

import java.sql
import java.sql.DriverManager
import java.util.Properties

import scala.io.Source

object QuestAdder {

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

}
