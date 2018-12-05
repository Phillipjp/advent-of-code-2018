package adventOfCode
import scala.annotation.tailrec
object Day5 {

  def reactPolymer(polymer: String):String={

    @tailrec
    def react(polymer: String, i: Int):String={
      //println(polymer)
      if(i == polymer.length-1) polymer
      else{
          if((Character.isUpperCase(polymer(i)) && Character.toLowerCase(polymer(i)) == polymer(i+1)) ||
            (Character.isLowerCase(polymer(i)) && Character.toUpperCase(polymer(i)) == polymer(i+1))){
            if (i == 0) react(polymer.substring(i + 2), i)
            else  react(polymer.substring(0, i) + polymer.substring(i + 2), i - 1)
          }
          else react(polymer, i+1)
      }
    }

    react(polymer, 0)
  }

  def reactPolymerWithMissingUnit(polymer: String, unit: Char):String={

    val polymerWithoutLowerUnit = polymer.replace(unit.toString, "")
    val polymerWithoutUnit = polymerWithoutLowerUnit.replace(Character.toUpperCase(unit).toString, "")

    @tailrec
    def react(polymer: String, i: Int):String={
      //println(polymer)
      if(i == polymer.length-1) polymer
      else{
        if((Character.isUpperCase(polymer(i)) && Character.toLowerCase(polymer(i)) == polymer(i+1)) ||
          (Character.isLowerCase(polymer(i)) && Character.toUpperCase(polymer(i)) == polymer(i+1))){
          if (i == 0) react(polymer.substring(i + 2), i)
          else  react(polymer.substring(0, i) + polymer.substring(i + 2), i - 1)
        }
        else react(polymer, i+1)
      }
    }

    react(polymerWithoutUnit, 0)
  }

  def main(args: Array[String]): Unit = {
    val polymer = Utils.readInPolymer("day5input.txt")

    //Part 1
    val reactedPolymer = reactPolymer(polymer)
    println(reactedPolymer.length)

    //Part 2
    val letters: Seq[Char] = 'a' to 'z'
    val min = letters.map(x => reactPolymerWithMissingUnit(polymer, x).size).min
    println(min)

  }
}
