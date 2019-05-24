package utils.parsing

import java.io.ByteArrayInputStream

import akka.util.ByteString

trait InGameParse extends DataFunc with MathUtils {

  def parsePocket104(data: ByteString): ParsedData104 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val Email = readUtfString(arr.readNBytes(40))
    arr.close()
    ParsedData104(Email)
  }
  case class  ParsedData104(email: String)

  def parsePocket133(data: ByteString): ParsedData133 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(4)
    val charId = readUInteger(arr.readNBytes(4))
    arr.readNBytes(5)
    val arg1 = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData133(charId, arg1)
  }
  case class  ParsedData133(charId: Long, arg1:Short)

  def parsePocket137(data: ByteString): ParsedData137 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(3)
    val x = readUByte(arr.readNBytes(1)(0))
    val y = readUByte(arr.readNBytes(1)(0))
    val dir = readUByte(arr.readNBytes(1)(0))
    //val cord: Cords3 = GetEncoded3(arr.readNBytes(3))
    arr.close()
    ParsedData137(x, y, dir)
  }
  case class ParsedData137(x: Short, y: Short, dir: Short)

  def parsePocket144(data: ByteString): ParsedData144 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val charId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData144(charId)
  }
  case class  ParsedData144(charId:Long)

  def parsePocket148(data: ByteString): ParsedData148 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(6)
    val  ItemsId = readUShort(arr.readNBytes(2))
    arr.readNBytes(2)
    val  loc3 = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData148(ItemsId, loc3)
  }
  case class  ParsedData148(itemId: Int, loc3: Int)

  def parsePocket155(data: ByteString): ParsedData155 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    arr.readNBytes(4)
    val charId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData155(charId)
  }
  case class  ParsedData155(charId: Long)

  def parsePocket159(data: ByteString): ParsedData159 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val message_count = readUShort(arr.readNBytes(2))
    val message = readUtfString(arr.readNBytes(message_count - 5))
    arr.close()
    ParsedData159(message_count, message)
  }
  case class ParsedData159(message_count: Int, message: String)

  def parsePocket162(data: ByteString): ParsedData162 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    arr.readNBytes(10)
    val charId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData162(charId)
  }
  case class  ParsedData162(charId: Long)

  def parsePocket169(data: ByteString): ParsedData169 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val ItemId = readUShort(arr.readNBytes(2))
    val EquippedId = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData169(ItemId, EquippedId)
  }
  case class ParsedData169(ItemId: Int, EquippedId: Int)

  def parsePocket171(data: ByteString): ParsedData171 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val ItemId = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData171(ItemId)
  }
  case class ParsedData171(ItemId: Int)

  def parsePocket178(data: ByteString): ParsedData178 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    //val nulled = readUByte(arr.readNBytes(1)(0)) Почему то там первый шорт 8 битный
    val nulled = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData178(nulled)
  }
  case class  ParsedData178(nulled:Short)

  def parsePocket184(data: ByteString): ParsedData184 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val charId = readUInteger(arr.readNBytes(4))
    val nulled = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData184(charId, nulled)
  }
  case class  ParsedData184(charId: Long, nulled: Short)

  def parsePocket185(data: ByteString): ParsedData185 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val charId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData185(charId)
  }
  case class  ParsedData185(charId : Long)

  def parsePocket187(data: ByteString): ParsedData187 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val stat = readUShort(arr.readNBytes(2))
    val stats = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData187(stat, stats)
  }
  case class  ParsedData187(stat: Int, stats: Short)

  def parsePocket191(data: ByteString): ParsedData191 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val smiles = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData191(smiles)
  }
  case class  ParsedData191(smiles: Short)

  def parsePocket197(data: ByteString): ParsedData197 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val _npcStoreId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData197(_npcStoreId)
  }
  case class  ParsedData197(npcStoreId: Long)

  def parsePocket201(data: ByteString): ParsedData201 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val  Item_count = readUShort(arr.readNBytes(2))
    println("parsePocket201: " + ((Item_count - 4) / 4))
    val  Items_id = readUShort(arr.readNBytes(2))
    val  Items_Amount = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData201(Item_count, Items_id, Items_Amount)
  }
  case class  ParsedData201(itemCount: Int, itemId: Int, itemAmount: Int)

  def parsePocket207(data: ByteString): ParsedData207 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val NIckName = readUtfString(arr.readNBytes(24))
    val int1 = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData207(NIckName, int1)
  }
  case class  ParsedData207(nickName:String, int1: Short)

  def parsePocket208(data: ByteString): ParsedData208 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData208(int1)
  }
  case class  ParsedData208(int1: Short)

  def parsePocket245(data: ByteString): ParsedData245 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    arr.readNBytes(3)
    val login = readUInteger(arr.readNBytes(4))
    val lang = readUByte(arr.readNBytes(1)(0))
    arr.readNBytes(4)
    val map = readUInteger(arr.readNBytes(4))
    arr.readNBytes(6)
    val timer = readUInteger(arr.readNBytes(4))
    val sex = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData245(login,lang,map,timer,sex)
  }
  case class ParsedData245(loginId: Long, lang: Short, mapCharId: Long, timer: Long, sex: Short)

  def parsePocket252(data: ByteString): ParsedData252 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val CharId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData252(CharId)
  }
  case class  ParsedData252(charId: Long)

  def parsePocket255(data: ByteString): ParsedData255 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUInteger(arr.readNBytes(4))
    val int2 = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData255(int1, int2)
  }
  case class  ParsedData255(int1: Long, int2: Long)

  def parsePocket258(data: ByteString): ParsedData258 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val  SendPartyChangeOption = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData258(SendPartyChangeOption)
  }
  case class  ParsedData258(partyChaneOption: Int)

  def parsePocket259(data: ByteString): ParsedData259 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val CharId = readUInteger(arr.readNBytes(4))
    val PlayerName = readUtfString(arr.readNBytes(24))
    arr.close()
    ParsedData259(CharId, PlayerName)
  }
  case class  ParsedData259(charId: Long, playerName: String)

  def parsePocket278(data: ByteString): ParsedData278 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val initialSync = readUShort(arr.readNBytes(2))
    val getTimers = readUInteger(arr.readNBytes(4))
    val nulled = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData278(initialSync, getTimers, nulled)
  }
  case class ParsedData278(initialSync: Int, getTimers: Long, nulled: Short)

  def parsePocket323(data: ByteString): ParsedData323 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUInteger(arr.readNBytes(4))
    val int2 = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData323(int1, int2)
  }
  case class  ParsedData323(int1: Long, int2: Long)

  def parsePocket326(data: ByteString): ParsedData326 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val charId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData326(charId)
  }
  case class  ParsedData326(charId: Long)

  def parsePocket376(data: ByteString): ParsedData376 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val  ItemsId = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData376(ItemsId)
  }
  case class  ParsedData376(itemsId: Int)

  def parsePocket380(data: ByteString): ParsedData380 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUShort(arr.readNBytes(2))
    val int2 = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData380(int1, int2)
  }
  case class  ParsedData380(int1: Int, int2: Int)

  def parsePocket394(data: ByteString): ParsedData394 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val short = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData394(short)
  }
  case class  ParsedData394(short: Int)

  def parsePocket469(data: ByteString): ParsedData469 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val string_length = readUShort(arr.readNBytes(2))
    val NPCId = readUInteger(arr.readNBytes(4))
    val string = readUtfString(arr.readNBytes(string_length - 9))
    arr.close()
    ParsedData469(NPCId, string)
  }
  case class  ParsedData469(npcId: Long, string: String)

  def parsePocket488(data: ByteString): ParsedData488 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val PartyName = readUtfString(arr.readNBytes(24))
    arr.readNBytes(2)
    arr.close()
    ParsedData488(PartyName)
  }
  case class  ParsedData488(partyName: String)

  def parsePocket514(data: ByteString): ParsedData514 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val FriendsNIckNameAdd = readUtfString(arr.readNBytes(24))
    arr.close()
    ParsedData514(FriendsNIckNameAdd)
  }
  case class  ParsedData514(friendsNickNameAdd: String)

  def parsePocket515(data: ByteString): ParsedData515 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUInteger(arr.readNBytes(4))
    val int2 = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData515(int1, int2)
  }
  case class  ParsedData515(int1: Long, int2: Long)

  def parsePocket520(data: ByteString): ParsedData520 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUInteger(arr.readNBytes(4))
    val int2 = readUInteger(arr.readNBytes(4))
    val int3 = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData520(int1, int2, int3)
  }
  case class  ParsedData520(int1: Long, int2: Long, int3: Long)

  def parsePocket726(data: ByteString): ParsedData726 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val charId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData726(charId)
  }
  case class  ParsedData726(charId : Long)

  def parsePocket728(data: ByteString): ParsedData728 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    arr.readNBytes(4)
    val charId = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData728(charId)
  }
  case class  ParsedData728(charId: Long)

  def parsePocket1032(data: ByteString): ParsedData1032 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val NickName = readUtfString(arr.readNBytes(24))
    arr.close()
    ParsedData1032(NickName)
  }
  case class  ParsedData1032(nickName: String)

  def parsePocket1037(data: ByteString): ParsedData1037 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val arg1 = readUInteger(arr.readNBytes(4))
    val arg2 = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData1037(arg1, arg2)
  }
  case class  ParsedData1037(arg1: Long, arg2: Long)

  def parsePocket1039(data: ByteString): ParsedData1039 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUInteger(arr.readNBytes(4))
    val int2 = readUInteger(arr.readNBytes(4))
    val int3 = readUInteger(arr.readNBytes(4))
    val int4 = readUInteger(arr.readNBytes(4))
    val int5 = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData1039(int1, int2, int3, int4, int5)
  }
  case class  ParsedData1039(int1: Long, int2: Long, int3: Long, int4: Long, int5: Long)

  def parsePocket1041(data: ByteString): ParsedData1041 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUShort(arr.readNBytes(2))
    val int2 = readUShort(arr.readNBytes(2))
    val arg1 = readUByte(arr.readNBytes(1)(0))
    arr.close()
    ParsedData1041(int1, int2, arg1)
  }
  case class  ParsedData1041(int1: Int, int2: Int, arg1: Short)

  def parsePocket1042(data: ByteString): ParsedData1042 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val int1 = readUInteger(arr.readNBytes(4))
    val int2 = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData1042(int1, int2)
  }
  case class  ParsedData1042(int1: Long, int2: Long)

  def parsePocket1070(data: ByteString): ParsedData1070 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val  SendAutoSellItem = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData1070(SendAutoSellItem)
  }
  case class  ParsedData1070(autoSellItem: Int)

  def parsePocket1072(data: ByteString): ParsedData1072 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val  smiles = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData1072(smiles)
  }
  case class  ParsedData1072(smiles: Int)

  def parsePocket1095(data: ByteString): ParsedData1095 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val  SenBuyZeny = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData1095(SenBuyZeny)
  }
  case class  ParsedData1095(senBuyZen: Int)

  def parsePocket1096(data: ByteString): ParsedData1096 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val local5 = readUShort(arr.readNBytes(2))
    val str1 = readUByte(arr.readNBytes(1)(0))
    val str2 = readUtfString(arr.readNBytes(32))
    arr.readNBytes(1)
    val fin = arr.readAllBytes()
    val local4 = readUtfString(fin.slice(0, fin.length - 2))
    arr.close()
    ParsedData1096(local5, str1, str2, local4)
  }
  case class ParsedData1096(local5: Int, str1: Short, str2: String, local4: String)

  def parsePocket1115(data: ByteString): ParsedData1115 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(2)
    val string = readUtfString(arr.readNBytes(50))
    arr.close()
    ParsedData1115(string)
  }
  case class  ParsedData1115(str: String)

  def parsePocket524488(data: ByteString): ParsedData524488 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(4)
    val  item = readUShort(arr.readNBytes(2))
    val  items = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData524488(item, items)
  }
  case class  ParsedData524488(item: Int, items: Int)

  def parsePocket524489(data: ByteString): ParsedData524489 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(4)
    val  item = readUShort(arr.readNBytes(2))
    val  items = readUShort(arr.readNBytes(2))
    arr.close()
    ParsedData524489(item, items)
  }
  case class  ParsedData524489(item: Int, items: Int)

  def parsePocket912588914(data: ByteString): ParsedData912588914 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(5)
    val  ItemsId = readUShort(arr.readNBytes(2))
    arr.readNBytes(2)
    val charid = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData912588914(ItemsId, charid)
  }
  case class  ParsedData912588914(itemsId: Int, charId: Long)

  def parsePocket1616970003(data: ByteString): ParsedData1616970003 = {
    val arr = new ByteArrayInputStream(data.toArray)
    arr.readNBytes(5)
    val item = readUInteger(arr.readNBytes(4))
    arr.close()
    ParsedData1616970003(item)
  }
  case class  ParsedData1616970003(item: Long)


  //читает первый Шорт
  def pocketNumber(data:ByteString): Int = {
    val buf = new ByteArrayInputStream(data.toArray)
    val out = readUShort(buf.readNBytes(2))
    buf.close()
    out
  }

}
