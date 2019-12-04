package day01

import scala.io.Source

object Day01_2
{
  def fuelRequirements(mass: Int) : Int =
  {
    val fuelRequired = mass / 3 - 2

    if (fuelRequired < 0)
      0
    else
    {
      fuelRequired + fuelRequirements(fuelRequired)
    }
  }

  def totalFuelRequirements(moduleMasses: Seq[String]) : Int =
  {
    moduleMasses.map(m => fuelRequirements(m.toInt)).sum
  }


  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(totalFuelRequirements(Seq("14")) == 2)
      assert(totalFuelRequirements(Seq("1969")) == 966)
      assert(totalFuelRequirements(Seq("100756")) == 50346)
    }

    // Read input
    //
    val freqChanges = Source.fromResource("day01.txt").getLines.toSeq

    println(totalFuelRequirements(freqChanges))
  }
}
