package day09

import day09.Day09_1.runProgram

import scala.io.Source

object Day09_2
{
  def main(args: Array[String]): Unit =
  {
    // Read input
    //
    val program = Source.fromResource("day09.txt").getLines.toSeq(0)
    println(runProgram(program, Seq(2)))
  }
}
