package day07

import scala.collection.mutable.Buffer
import scala.io.Source

object Status extends Enumeration {
  type Status = Value
  val WaitingForInput, Exit = Value
}
import Status._

case class Halt(status: Status, outputs: Seq[Int], haltPos: Int)

class Amp(instructions: Seq[Int], val letter: String, phase: Int)
{
  val memory: Buffer[Int] = instructions.toBuffer
  var pos = 0

  def run(inputs: Seq[Int]) : Halt =
  {
    val inputsWithPhase = if (pos == 0) Seq(phase) ++ inputs else inputs

    if (false) println(f"Running Amp $letter with inputs $inputsWithPhase")

    val halt = IntegerComputer.processInstruction(memory, pos, inputsWithPhase, Seq())
    pos = halt.haltPos

    if (false)
    {
      val status = halt.status
      val outputs = halt.outputs
      println(f"Halted at position $pos due to $status with outputs $outputs")
    }

    halt
  }
}

object IntegerComputer
{
  def getModes(modeNumber: Int): Array[Boolean] =
  {
    Array(modeNumber % 10 == 1, (modeNumber / 10) % 10 == 1, (modeNumber / 100) % 10 == 1)
  }

  def getParameterValue(p: Int, memory: Buffer[Int], pos: Int, modes: Array[Boolean]): Int =
  {
    if (modes(p - 1)) memory(pos + p) else memory(memory(pos + p))
  }

  def processInstruction(memory: Buffer[Int], pos: Int, inputs: Seq[Int], outputs: Seq[Int]): Halt =
  {
    val instruction = memory(pos)
    val opcode = instruction % 100
    val modes = getModes(instruction / 100)

    opcode match {
      case 1 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        val val2 = getParameterValue(2, memory, pos, modes)
        val dest = memory(pos + 3)
        memory(dest) = val1 + val2
        processInstruction(memory, pos + 4, inputs, outputs)
      case 2 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        val val2 = getParameterValue(2, memory, pos, modes)
        val dest = memory(pos + 3)
        memory(dest) = val1 * val2
        processInstruction(memory, pos + 4, inputs, outputs)
      case 3 =>
        if (inputs.isEmpty)
          Halt(WaitingForInput, outputs, pos)
        else
        {
          memory(memory(pos + 1)) = inputs.head
          processInstruction(memory, pos + 2, inputs.tail, outputs)
        }
      case 4 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        processInstruction(memory, pos + 2, inputs, outputs ++ Seq(val1))
      case 5 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        val val2 = getParameterValue(2, memory, pos, modes)
        if (val1 != 0)
          processInstruction(memory, val2, inputs, outputs)
        else
          processInstruction(memory, pos + 3, inputs, outputs)
      case 6 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        val val2 = getParameterValue(2, memory, pos, modes)
        if (val1 == 0)
          processInstruction(memory, val2, inputs, outputs)
        else
          processInstruction(memory, pos + 3, inputs, outputs)
      case 7 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        val val2 = getParameterValue(2, memory, pos, modes)
        val dest = memory(pos + 3)
        memory(dest) = if (val1 < val2) 1 else 0
        processInstruction(memory, pos + 4, inputs, outputs)
      case 8 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        val val2 = getParameterValue(2, memory, pos, modes)
        val dest = memory(pos + 3)
        memory(dest) = if (val1 == val2) 1 else 0
        processInstruction(memory, pos + 4, inputs, outputs)
      case 99 =>
        Halt(Exit, outputs, -1)
      case _ =>
        assert(false, "Unknown instruction")
        Halt(Exit, outputs, -1)
    }
  }
}


object Day07_1
{
  def getThrusterSignal(instructions: Seq[Int], phases: Seq[Int]): Int =
  {
    // Create amps
    val amps = phases.zip(Seq("A", "B", "C", "D", "E")).map { case (phase, letter) => new Amp(instructions, letter, phase)}

    // Initial input to first amp
    var outputs = Seq(0)

    for (i <- 0 until amps.size)
    {
      // Run amp until it halts
      val halt = amps(i).run(outputs)
      outputs = halt.outputs           // store output from amp
    }

    outputs.head
  }

  def findMaxThrusterSignalCore(instructions: Seq[Int], phasePermutations: Seq[Seq[Int]], maxThrusterSignal: Int): Int =
  {
    if (phasePermutations.isEmpty)
      maxThrusterSignal
    else
    {
      // Compute thruster signal for this specific phase permutation
      val thrusterSignal = getThrusterSignal(instructions, phasePermutations.head)

      // Keep max thruster signal and compute thruster signal for next permutation
      findMaxThrusterSignalCore(instructions, phasePermutations.tail, Math.max(thrusterSignal, maxThrusterSignal))
    }
  }

  def findMaxThrusterSignal(input: String): Int =
  {
    val instructions = input.split(',').toSeq.map(_.toInt)
    val phaseSequences = Seq(0, 1, 2, 3, 4).permutations.toSeq

    findMaxThrusterSignalCore(instructions, phaseSequences, Int.MinValue)
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findMaxThrusterSignal("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") == 43210)
      assert(findMaxThrusterSignal("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0") == 54321)
      assert(findMaxThrusterSignal("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") == 65210)
      println("Tests ok!")
    }

    // Read input
    //
    val program = Source.fromResource("day07.txt").getLines.toSeq(0)

    println(findMaxThrusterSignal(program))
  }

}
