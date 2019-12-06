package day06

import scala.io.Source

object Day06_1
{
  def numOrbits(orbitMap: Map[String, String], key: String, sumOrbits: Int): Int =
  {
    if (!orbitMap.contains(key))
      sumOrbits
    else
      numOrbits(orbitMap, orbitMap(key), sumOrbits + 1)
  }

  def countOrbits(input: Seq[String]): Int =
  {
    val seqOfArrayPairs = input.map(_.split(')'))
    val seqOfTuples = seqOfArrayPairs.map { case Array(f1,f2) => (f2,f1) }
    val orbitMap = seqOfTuples.toMap
    orbitMap.keys.toSeq.map(k => numOrbits(orbitMap, k, 0)).sum
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(countOrbits(Seq("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")) == 42)
      println("Tests ok!")
    }

    // Read input
    //
    val input = Source.fromResource("day06.txt").getLines.toSeq

    println(countOrbits(input))
  }
}
