package day05

import scala.collection.mutable.Buffer
import scala.io.Source

object Day05_1
{
  def getModes(modeNumber: Int): Array[Boolean] =
  {
    Array(modeNumber % 10 == 1, (modeNumber / 10) % 10 == 1, (modeNumber / 100) % 10 == 1)
  }

  def getParameterValue(p: Int, memory: Buffer[Int], pos: Int, modes: Array[Boolean]): Int =
  {
    if (modes(p - 1)) memory(pos + p) else memory(memory(pos + p))
  }

  def processInstruction(memory: Buffer[Int], pos: Int, input: Int): Unit =
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
        processInstruction(memory, pos + 4, input)
      case 2 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        val val2 = getParameterValue(2, memory, pos, modes)
        val dest = memory(pos + 3)
        memory(dest) = val1 * val2
        processInstruction(memory, pos + 4, input)
      case 3 =>
        memory(memory(pos + 1)) = input
        processInstruction(memory, pos + 2, input)
      case 4 =>
        val val1 = getParameterValue(1, memory, pos, modes)
        println(val1)
        processInstruction(memory, pos + 2, input)
      case 99 =>
        return
      case _ =>
        assert(false, "Unknown instruction")
    }
  }

  def intComputer(program: Seq[Int]) : Unit =
  {
    processInstruction(program.toBuffer, 0, 1)
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(getModes(1).sameElements(Array(true, false, false)))
      assert(getModes(10).sameElements(Array(false, true, false)))
      assert(getModes(11).sameElements(Array(true, true, false)))
      assert(getModes(100).sameElements(Array(false, false, true)))
      assert(getModes(101).sameElements(Array(true, false, true)))
      assert(getModes(110).sameElements(Array(false, true, true)))
      assert(getModes(111).sameElements(Array(true, true, true)))
      println("Tests ok!")
    }

    // Read input
    //
    val program = Source.fromResource("day05.txt").getLines.toSeq(0).split(',').toSeq.map(_.toInt)

    intComputer(program)
  }
}
