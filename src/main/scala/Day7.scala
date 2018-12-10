package adventOfCode

import scala.annotation.tailrec

object Day7 {

  private def findDependentSteps(step: String, instructions: Seq[InstructionStep]):(String, Seq[String])={
    (step, instructions.filter(_.step == step).map(_.dependency))
  }

  def orderInstructionSteps(stepsSet: Set[String], stepDependencies: Set[(String, Seq[String])]): Seq[String] = {

    val stepOrder = Seq.empty[String]

    @tailrec
    def orderSteps(stepsSet: Set[String], stepDependencies: Set[(String, Seq[String])], stepOrder: Seq[String]): Seq[String] = {
      val nextStep = ((stepsSet -- stepDependencies.flatMap(_._2)) -- stepOrder.toSet).toSeq.sortWith(_<_).head
      if((stepsSet -- Seq(nextStep)).isEmpty)
        stepOrder ++ Seq(nextStep)
      else{
        orderSteps(stepsSet -- Seq(nextStep), stepDependencies.filter(s => s._1 != nextStep), stepOrder ++ Seq(nextStep))
      }
    }

    orderSteps(stepsSet, stepDependencies, stepOrder)
  }

  def main(args: Array[String]): Unit = {
    val instructions = Utils.readInstructionSteps("day7input.txt")

    val stepsSet = instructions.flatMap(s => List(s.step, s.dependency)).toSet
    val stepDependencies: Set[(String, Seq[String])] = stepsSet.map(findDependentSteps(_, instructions))

    val stepOrder = orderInstructionSteps(stepsSet, stepDependencies)

    stepOrder.foreach(print)


  }
}
case class InstructionStep(
                            step: String,
                            dependency: String
                      ) extends Ordered[InstructionStep] {

  override def compare(that: InstructionStep): Int = {
    this.step.compareTo(that.step)
  }
}