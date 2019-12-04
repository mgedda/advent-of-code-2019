package day02

import scala.collection.mutable.Buffer
import scala.io.Source

object Day02_2
{
  def processInstruction(memory: Buffer[Int], pos: Int) : Buffer[Int] =
  {
    memory(pos) match {
      case 1 => {
        memory(memory(pos + 3)) = memory(memory(pos + 1)) + memory(memory(pos + 2))
        processInstruction(memory, pos + 4)
      }
      case 2 => {
        memory(memory(pos + 3)) = memory(memory(pos + 1)) * memory(memory(pos + 2))
        processInstruction(memory, pos + 4)
      }
      case 99 => memory
      case _ => {
        assert(false, "Unknown instruction")
        Buffer()
      }
    }
  }

  def repair(program: Buffer[Int], noun: Int, verb: Int): Buffer[Int] =
  {
    program(1) = noun
    program(2) = verb
    program
  }

  def intComputer(program: Seq[Int]) : Int =
  {
    val res = for {
      noun <- 0 to 99
      verb <- 0 to 99
      if processInstruction(repair(program.toBuffer, noun, verb), 0)(0) == 19690720
    } yield 100 * noun + verb

    res(0)
  }

  def main(args: Array[String]): Unit =
  {
    // Read input
    //
    val program = Source.fromResource("day02.txt").getLines.toSeq(0).split(',').toSeq.map(_.toInt)

    println(intComputer(program))
  }
}
