package day02

import scala.collection.mutable.Buffer
import scala.io.Source

object Day02_1
{
  def processInstruction(buf: Buffer[Int], pos: Int) : Buffer[Int] =
  {
    buf(pos) match {
      case 1 => {
        buf(buf(pos + 3)) = buf(buf(pos + 1)) + buf(buf(pos + 2))
        processInstruction(buf, pos + 4)
      }
      case 2 => {
        buf(buf(pos + 3)) = buf(buf(pos + 1)) * buf(buf(pos + 2))
        processInstruction(buf, pos + 4)
      }
      case 99 => buf
      case _ => {
        assert(false, "Unknown instruction")
        Buffer()
      }
    }
  }

  def intComputer(program: Seq[Int]) : Seq[Int] =
  {
    val buffer = program.toBuffer
    buffer(1) = 12
    buffer(2) = 2
    processInstruction(buffer, 0).toSeq
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      /*
      assert(intComputer(Seq(1,0,0,0,99)) == Seq(2,0,0,0,99))
      assert(intComputer(Seq(2,3,0,3,99)) == Seq(2,3,0,6,99))
      assert(intComputer(Seq(2,4,4,5,99,0)) == Seq(2,4,4,5,99,9801))
      assert(intComputer(Seq(1,1,1,4,99,5,6,0,99)) == Seq(30,1,1,4,2,5,6,0,99))
      assert(intComputer(Seq(1,9,10,3,2,3,11,0,99,30,40,50)) == Seq(3500,9,10,70,2,3,11,0,99,30,40,50))
      */
    }

    // Read input
    //
    val program = Source.fromResource("day02.txt").getLines.toSeq(0).split(',').toSeq.map(_.toInt)

    println(intComputer(program)(0))
  }

}
