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
    //Part 1
    val squaresPerClaim = plans.flatMap(x => planClaim(x._2, x._3, x._4,x._5))
      .groupBy(identity).mapValues(_.size)
      .filter(_._2 > 1)
    val repeatedSquares = squaresPerClaim.size
    println(repeatedSquares)

    //Part 2
    val allPatterns = plans.map(x => (x._1, planClaim(x._2, x._3, x._4,x._5).toSet))

    val uniqueSquares = plans.flatMap(x => planClaim(x._2, x._3, x._4,x._5))
      .groupBy(identity).mapValues(_.size)
      .filter(_._2  == 1).keys.toSet

    val uniquePattern = allPatterns.map(x => (x._1, x._2--uniqueSquares)).filter(_._2.isEmpty).map(_._1).head
    println(uniquePattern)






  }
}
