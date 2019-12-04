package day01

import scala.io.Source

object Day01_1
{
  def totalFuelRequirements(moduleMasses: Seq[String]) : Int =
  {
    moduleMasses.map(_.toInt / 3 - 2).sum
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(totalFuelRequirements(Seq("12")) == 2)
      assert(totalFuelRequirements(Seq("14")) == 2)
      assert(totalFuelRequirements(Seq("1969")) == 654)
      assert(totalFuelRequirements(Seq("100756")) == 33583)
      assert(totalFuelRequirements(Seq("12", "14", "1969", "100756")) == Seq(2, 2, 654, 33583).sum)
    }

    // Read input
    //
    val freqChanges = Source.fromResource("day01.txt").getLines.toSeq

    println(totalFuelRequirements(freqChanges))
  }
}
