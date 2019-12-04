package day03

import scala.io.Source

object Day03_1
{
  def expandInstructions(instructionsLeft: Seq[String], path: Seq[(Int, Int)]): Seq[(Int, Int)] =
  {
    if (instructionsLeft.isEmpty)
    {
      path
    }
    else
    {
      val (xPos, yPos) = path.last

      val direction = instructionsLeft(0).substring(0,1)
      val numSteps = instructionsLeft(0).substring(1).toInt

      val instructionPath = direction match {
        case "U" => for {y <- (yPos + 1) to (yPos + numSteps)} yield (xPos, y)
        case "D" => for {y <- (yPos - 1) to (yPos - numSteps) by -1} yield (xPos, y)
        case "R" => for {x <- (xPos + 1) to (xPos + numSteps)} yield (x, yPos)
        case "L" => for {x <- (xPos - 1) to (xPos - numSteps) by -1} yield (x, yPos)
      }

      expandInstructions(instructionsLeft.tail, path ++ instructionPath)
    }
  }

  def findIntersections(positions1: Seq[(Int, Int)], positions2: Seq[(Int, Int)], intersections: Seq[(Int, Int)]) : Seq[(Int, Int)] =
  {
    if (positions1.isEmpty || positions2.isEmpty)
    {
      intersections
    }
    else
    {
      val x = positions1.head._1
      val subPositions1 = positions1.filter(p => p._1 == x)
      val subPositions2 = positions2.filter(p => p._1 == x)

      val subIntersections = subPositions2.filter(p => subPositions1.contains(p))

      val positions1Left = positions1.filterNot(p => p._1 == x)
      val positions2Left = positions2.filterNot(p => p._1 == x)

      findIntersections(positions1Left, positions2Left, intersections ++ subIntersections)
    }
  }

  def formatWireInput(wireInput: String): Seq[(Int, Int)] =
  {
    val instructions = wireInput.split(',').toSeq
    expandInstructions(instructions, Seq((0,0)))
  }

  def intersectionDistance(input: Seq[String]) : Int =
  {
    val wire1Steps = formatWireInput(input(0))
    val wire2Steps = formatWireInput(input(1))

    val intersections = findIntersections(wire1Steps.tail, wire2Steps.tail, Seq())

    intersections.map(pos => Math.abs(pos._1) + Math.abs(pos._2)).min
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(intersectionDistance(Seq("R8,U5,L5,D3", "U7,R6,D4,L4")) == 6)
      assert(intersectionDistance(
        Seq("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")) == 159)
      assert(intersectionDistance(
        Seq("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")) == 135)
      println("Tests ok!")
    }

    // Read input
    //
    val input = Source.fromResource("day03.txt").getLines.toSeq

    println(intersectionDistance(input))
  }
}
