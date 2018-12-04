package adventOfCode
object Day2 {

  private def stringContainsNChar(n: Int, s: String, c: Char):Boolean = {
    s.count(_ == c) == n
  }
  def containsRepeatedCharacters(s: String):(Int, Int) = {
    val charSet = s.toCharArray.toSet

    val a = if(charSet.count(stringContainsNChar(2, s, _)) > 0) 1
    else 0
    val b = if(charSet.count(stringContainsNChar(3, s, _)) > 0) 1
    else 0

    (a,b)
  }

  def removeCharFromEveryIndex(s: String): Seq[String] = {
    for( i <- 0 to s.length-1) yield new StringBuilder(s).deleteCharAt(i).toString()
  }

  def main(args: Array[String]): Unit = {
    val codes = Utils.fileToSeqString("day2input.txt")
    //Part 1
    val x = codes.map(containsRepeatedCharacters)
    val a = x.map(_._1).sum
    val b = x.map(_._2).sum
    println(a*b)

    //Part 2
    val mapOfAllStringPermutations = codes.map(removeCharFromEveryIndex).transpose.map(_.groupBy(identity).mapValues(_.size).filter(_._2 == 2))
    val id = mapOfAllStringPermutations.filter(_.nonEmpty).flatMap(_.map(_._1)).head
    println(id)


  }
}
