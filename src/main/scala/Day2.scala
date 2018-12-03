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

  def main(args: Array[String]): Unit = {
    val codes = Utils.fileToSeqString("day2input.txt")
    val x = codes.map(containsRepeatedCharacters(_))
    val a = x.map(_._1).sum
    val b = x.map(_._2).sum
    println(a*b)
  }
}
