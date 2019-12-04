package day03

import day03.Day03_1.{findIntersections, formatWireInput}

import scala.io.Source

object Day03_2
{
  def fewestStepsToFirstIntersection(input: Seq[String]) : Int =
  {
    val wire1Steps = formatWireInput(input(0))
    val wire2Steps = formatWireInput(input(1))

    val intersections = findIntersections(wire1Steps.tail, wire2Steps.tail, Seq())

    intersections.map(pos => wire1Steps.indexOf(pos) + wire2Steps.indexOf(pos)).min
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(fewestStepsToFirstIntersection(Seq("R8,U5,L5,D3", "U7,R6,D4,L4")) == 30)
      assert(fewestStepsToFirstIntersection(
        Seq("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")) == 610)
      assert(fewestStepsToFirstIntersection(
        Seq("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) == 410)
      println("Tests ok!")
    }

    // Read input
    //
    val input = Source.fromResource("day03.txt").getLines.toSeq

    println(fewestStepsToFirstIntersection(input))
  }

}
