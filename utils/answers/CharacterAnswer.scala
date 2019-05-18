package utils.answers

trait CharacterAnswer {

  import CharacterAnswer._

  def getJobFract(long: Long): JobId = long match {
    case 512 => JobId(2, 0)
    case 1024 => JobId(4, 0)
    case 256 => JobId(1, 0)
    case 1536 => JobId(6, 0)
    case 1280 => JobId(5, 0)
    case 768 => JobId(3, 0)
    case 1792 => JobId(4024, 0)
    case 16896 => JobId(2, 1)
    case 17408 => JobId(4, 1)
    case 16640 => JobId(1, 1)
    case 17920 => JobId(6, 1)
    case 17664 => JobId(5, 1)
    case 17152 => JobId(3, 1)
    case 18176 => JobId(4024, 1)
  }

}

object CharacterAnswer{
  case class JobId(jobId: Short, fraction: Short)
  case class CharStats(charId:Long, baseExp: Long, money: Long,
                       jobExp: Long, jobLvl: Int, hp: Int,
                       maxHp: Int,sp: Int, maxSp: Int,
                       jobId: Int, local3Sex: Int, baseLvl: Int,
                       hairColor: Int, clothesColor: Int, name: String,
                       str: Short, agi: Short, vit: Short,
                       int: Short, dex: Short, luk: Short,
                       slot: Int, renames: Int)
}
