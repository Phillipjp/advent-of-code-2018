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

}
