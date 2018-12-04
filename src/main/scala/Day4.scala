package adventOfCode

import java.time.LocalDateTime

import scala.annotation.tailrec

object Day4 {

  def sortRecord(a: GuardRecord, b: GuardRecord): Boolean = {
    a.compare(b) <= 0
  }

  def findGuardsSleepingTimes(records: Stream[GuardRecord]): Map[String, List[Int]] = {

    val guardsSleeping = Map.empty[String, List[Int]]

    @tailrec
    def findGuards(guardsSleeping: Map[String, List[Int]], stream: Stream[GuardRecord], id: String): Map[String, List[Int]] = {
      val pattern = "([#]\\d*\\b)".r
      if (stream.length == 0) {
        guardsSleeping
      }
      else {
        if (pattern.findFirstIn(stream.head.action).nonEmpty) {
          val currentId = pattern.findFirstIn(stream.head.action).get
          if (guardsSleeping.keySet.contains(currentId)) findGuards(guardsSleeping, stream.tail, currentId)
          else findGuards(guardsSleeping + (currentId -> List.empty[Int]), stream.tail, currentId)
        }
        else {
          findGuards(guardsSleeping + (id -> (guardsSleeping(id) :+ stream.head.timestamp.getMinute)), stream.tail, id)
        }
      }
    }

    findGuards(guardsSleeping, records, "#0")
  }

  def findMostCommonMinute(stream: Stream[List[Int]]): Int = {
    val minutes = List.empty[Int]

    @tailrec
    def mostCommonMinute(minutes: List[Int], stream: Stream[List[Int]]): Int = {
      stream.length match {
        case 0 => minutes.groupBy(x => x).mapValues(_.size).maxBy(_._2)._1
        case _ => stream.head match {
          case start :: end :: _ => {
            val range = start until end

            mostCommonMinute((minutes ++: range).toList, stream.tail)
          }
        }
      }
    }

    mostCommonMinute(minutes, stream)
  }

  def main(args: Array[String]): Unit = {

    //Part 1
    val records = Utils.readGuardRecords("day4input.txt")
    val sortedRecords = records.sortWith(sortRecord).toStream

    val sleepingTimes = findGuardsSleepingTimes(sortedRecords)

    val totalSleepingTimes = sleepingTimes.map(x => (x._1, x._2.grouped(2).toList.foldLeft(0)((x, y) => x + (y.last - y.head))))


    val sleepiestGuard = totalSleepingTimes.maxBy { case (key, value) => value }
    println(sleepiestGuard._1)

    val sleepiestGuardsSleepingRanges = sleepingTimes.map(x => (x._1, x._2.grouped(2).toList)).filter(_._1.equals(sleepiestGuard._1)).values.flatten.toStream

    val mostCommonMinute = findMostCommonMinute(sleepiestGuardsSleepingRanges)

    println(mostCommonMinute)

    //Part 2


  }
}

case class GuardRecord(
                        timestamp: LocalDateTime,
                        action: String
                      ) extends Ordered[GuardRecord] {

  override def compare(that: GuardRecord): Int = {
    this.timestamp.compareTo(that.timestamp)
  }
}
