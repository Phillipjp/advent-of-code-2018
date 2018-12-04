package adventOfCode
import Utils._
object Day3 {

  def planClaim(x: Int, y: Int, w: Int, h: Int):Seq[(Int,Int)] = {
    Stream.iterate((x,y))(nextSquare(_,w,h,x))
      .takeUntil(s => s._1 == x+w-1 && s._2 == y+h-1)
  }

  private def nextSquare(square: (Int, Int), w: Int, h: Int, sx: Int):(Int, Int) = {
    square match {
      case (x, y) => {
        if(x+1==sx+w) (sx, y+1)
        else (x+1, y)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val plans = Utils.fileToSeqElfPlans("day3input.txt")
    val squaresPerClaim = plans.flatMap(x => planClaim(x._2, x._3, x._4,x._5))
      .groupBy(identity).mapValues(_.size)
      .filter(_._2 > 1)
    val repeatedSquares = squaresPerClaim.size
    println(repeatedSquares)
  }
}
