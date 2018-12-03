import scala.annotation.tailrec
import scala.collection.mutable

object Day1 {



//  def findRepeats(runningTotals: Set[Long], currentTotal: Long, frequencies: Seq[Long], i: Int):Long = {
//    if(runningTotals.contains(currentTotal + frequencies(i))) return currentTotal + frequencies(i)
//    else if(i+1 < frequencies.length) {
//      findRepeats(runningTotals + (currentTotal + frequencies(i)), (currentTotal + frequencies(i)), frequencies, i + 1)
//    }
//    else{
//      findRepeats(runningTotals + (currentTotal + frequencies(i)), (currentTotal + frequencies(i)), frequencies, 0)
//    }
//  }

  def findRepeatsStream(stream: Stream[Long]): Long = {
    val runningTotals = Set.empty[Long]

    @tailrec
    def findRepeats(runningTotals: Set[Long], stream: Stream[Long]): Long ={
      if(runningTotals.contains(stream.head)) stream.head
      else findRepeats(runningTotals + stream.head, stream.tail)
    }

    return findRepeats(runningTotals, stream)
  }


  def main(args: Array[String]): Unit = {

    val frequencies = Utils.fileToSeqLong("day1input.txt")
    //println(frequencies.sum)

    val freq = Stream.continually(frequencies.toStream).flatten.scanLeft(0L)(_+_)

//    println(findRepeats(Set.empty[Long], 0, frequencies, 0))
    println(findRepeatsStream(freq))
  }
}
