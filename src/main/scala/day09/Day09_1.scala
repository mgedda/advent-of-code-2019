package day09

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Status extends Enumeration {
  type Status = Value
  val WaitingForInput, Exit = Value
}
import Status._

case class Halt(status: Status, outputs: Seq[Long], haltPos: Long)

object IntegerComputer
{
  def getModes(modeNumber: Int): Array[Int] =
  {
    Array(modeNumber % 10, (modeNumber / 10) % 10, (modeNumber / 100) % 10)
  }

  def getAddress(parameter: Int, memory: ArrayBuffer[Long], pos: Long, modes: Array[Int], relativeBase: Long): Int =
  {
    val mode = modes(parameter - 1)
    mode match {
      case 0 => memory(pos.toInt + parameter).toInt                   // position mode
      case 1 => pos.toInt + parameter                                 // immediate mode
      case 2 => (memory(pos.toInt + parameter) + relativeBase).toInt  // relative mode
    }
  }

  def processInstruction(memory: ArrayBuffer[Long], pos: Long, inputs: Seq[Long], outputs: Seq[Long], relativeBase: Long): Halt =
  {
    val instruction = memory(pos.toInt)
    val opcode = instruction % 100
    val modes = getModes(instruction.toInt / 100)

    opcode match {
      case 1 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        val addr2 = getAddress(2, memory, pos, modes, relativeBase)
        val addr3 = getAddress(3, memory, pos, modes, relativeBase)
        memory(addr3) = memory(addr1) + memory(addr2)
        processInstruction(memory, pos + 4, inputs, outputs, relativeBase)
      case 2 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        val addr2 = getAddress(2, memory, pos, modes, relativeBase)
        val addr3 = getAddress(3, memory, pos, modes, relativeBase)
        memory(addr3) = memory(addr1) * memory(addr2)
        processInstruction(memory, pos + 4, inputs, outputs, relativeBase)
      case 3 =>
        if (inputs.isEmpty)
          Halt(WaitingForInput, outputs, pos)
        else
        {
          val addr1 = getAddress(1, memory, pos, modes, relativeBase)
          memory(addr1) = inputs.head
          processInstruction(memory, pos + 2, inputs.tail, outputs, relativeBase)
        }
      case 4 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        val out = memory(addr1)
        processInstruction(memory, pos + 2, inputs, outputs ++ Seq(out), relativeBase)
      case 5 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        val addr2 = getAddress(2, memory, pos, modes, relativeBase)
        if (memory(addr1) != 0)
          processInstruction(memory, memory(addr2), inputs, outputs, relativeBase)
        else
          processInstruction(memory, pos + 3, inputs, outputs, relativeBase)
      case 6 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        val addr2 = getAddress(2, memory, pos, modes, relativeBase)
        if (memory(addr1) == 0)
          processInstruction(memory, memory(addr2), inputs, outputs, relativeBase)
        else
          processInstruction(memory, pos + 3, inputs, outputs, relativeBase)
      case 7 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        val addr2 = getAddress(2, memory, pos, modes, relativeBase)
        val addr3 = getAddress(3, memory, pos, modes, relativeBase)
        memory(addr3) = if (memory(addr1) < memory(addr2)) 1 else 0
        processInstruction(memory, pos + 4, inputs, outputs, relativeBase)
      case 8 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        val addr2 = getAddress(2, memory, pos, modes, relativeBase)
        val addr3 = getAddress(3, memory, pos, modes, relativeBase)
        memory(addr3) = if (memory(addr1) == memory(addr2)) 1 else 0
        processInstruction(memory, pos + 4, inputs, outputs, relativeBase)
      case 9 =>
        val addr1 = getAddress(1, memory, pos, modes, relativeBase)
        processInstruction(memory, pos + 2, inputs, outputs, relativeBase + memory(addr1))
      case 99 =>
        Halt(Exit, outputs, -1)
      case _ =>
        assert(false, "Unknown instruction")
        Halt(Exit, outputs, -1)
    }
  }
}

object Day09_1
{
  def runProgram(program: String, input: Seq[Long]): String =
  {
    val instructions = program.split(',').map(_.toLong)
    val buffer = ArrayBuffer.fill(100000)(0.toLong).prependAll(instructions)
    val halt = IntegerComputer.processInstruction(buffer, 0, input, Seq(), 0)
    halt.outputs.mkString(",")
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(runProgram("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99", Seq()) == "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
      assert(runProgram("1102,34915192,34915192,7,4,7,99,0", Seq()).length == 16)
      assert(runProgram("104,1125899906842624,99", Seq()) == "1125899906842624")
      println("Tests ok!")
    }

    // Read input
    //
    val program = Source.fromResource("day09.txt").getLines.toSeq(0)
    println(runProgram(program, Seq(1)))
  }
}
