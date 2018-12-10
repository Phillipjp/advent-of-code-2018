package adventOfCode
import Utils._

object Day6 {
  private def nextCoord(square: (Int, Int), maxX: Int, maxY: Int):(Int, Int) = {
    square match {
      case (x, y) => {
        if(x+1 > maxX) (0, y+1)
        else (x+1, y)
      }
    }
  }

  private def findClosestPoint(coord: (Int, Int), points: Map[String, (Int, Int)], maxX: Int, maxY: Int):(String, (Int,Int)) = {

    val distanceToPoints = points.map(p => (p._1, manhattanDistance(p._2, coord)))
      .toSeq
      .sortWith(_._2 < _._2)

    if (distanceToPoints(0)._2 == distanceToPoints(1)._2)
      (".", coord)
    else
      (distanceToPoints.head._1, coord)


  }

  private def totalDistanceLessThan10000(coord: (Int, Int), points: Map[String, (Int, Int)], maxX: Int, maxY: Int):String = {

    val distanceToPoints = points.map(p => manhattanDistance(p._2, coord)).sum
    if (distanceToPoints < 10000)
      "#"
    else
      "x"


  }

  private def manhattanDistance(a: (Int, Int), b: (Int, Int)):Int = {
    Math.abs(a._1-b._1)+Math.abs(a._2-b._2)
  }

  private def isInfinite(point: (Int, Int), maxX: Int, maxY: Int):Boolean = {
    point._1 == 0 || point._1 == maxX || point._2 == 0 || point._2 == maxY
  }

  def main(args: Array[String]): Unit = {
    val coords = Utils.readInIntTuples("day6input.txt")

    val maxX = coords.map(_._1).max
    val maxY = coords.map(_._2).max

    val letters: Seq[Int] = 1 to 100
    val coordsMap = (letters.map(_.toString) zip coords).toMap

    val grid = Stream.iterate((0,0))(nextCoord(_, maxX, maxY)).takeUntil(c => c._1 == maxX && c._2 == maxY)

    // Part 1
    val allPoints = grid.map(p => findClosestPoint(p, coordsMap, maxX, maxY)).filter(_._1 != ".")

    val infinites = allPoints.filter(p => isInfinite(p._2, maxX, maxY)).map(_._1).toSet
    val areaSizes = allPoints.groupBy(i => i._1).mapValues(_.size)
    val biggestArea = areaSizes.filter(p => !infinites.contains(p._1)).maxBy(_._2)
    println(biggestArea)

    // Part 2
    val points = grid.map(p => totalDistanceLessThan10000(p, coordsMap, maxX, maxY)).groupBy(i => i).mapValues(_.size).filter(_._1 == "#")
    println(points)


  }
}
