package app.PocketLogic

object CheckBase {
  //Здесь прописывает функции проверок

  def checkUserEmail = (email : String) => if (email.length > 25) false else true

}
