package adventOfCode
object Utils {

  def fileToSeqLong(fileName: String): Seq[Long] = {
    val bufferedSource = io.Source.fromResource(fileName)
    val lines = (for (line <- bufferedSource.getLines()) yield line.toLong).toList
    bufferedSource.close
    lines
  }

  def fileToSeqString(fileName: String): Seq[String] = {
    val bufferedSource = io.Source.fromResource(fileName)
    val lines = (for (line <- bufferedSource.getLines()) yield line.toString).toList
    bufferedSource.close
    lines
  }

  def fileToSeqElfPlans(fileName: String): Seq[(String, Int, Int, Int, Int)] = {
    val bufferedSource = io.Source.fromResource(fileName)
    val lines = (for (line <- bufferedSource.getLines()) yield line.toString).toList
    bufferedSource.close
    lines.map(elfPlan)
  }


  private def elfPlan(plan: String):(String, Int, Int, Int, Int) ={
    val split = plan.split(" ")
    val id = split(0)

    val start = split(2).split(",")
    val x = start(0).toInt

    val y = new StringBuilder(start(1)).deleteCharAt(start(1).length-1).toString().toInt
    val dim = split(3).split("x")
    val w = dim(0).toInt
    val h = dim(1).toInt

    (id, x,y, w, h)
  }

  implicit class TakeUntilIteratorWrapper[T](stream: Stream[T]) {
    def takeUntil(predicate: T => Boolean): Stream[T] = {
      stream.span(!predicate(_)) match {
        case (head, tail) => head append tail.take(1)
      }
    }
  }

}


