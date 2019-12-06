package day06

import scala.io.Source

object Day06_2
{
  def getOrbitalTransfersList(orbitMap: Map[String, String], distanceSeq: Seq[(String, Int)], key: String, distance: Int): Seq[(String, Int)] =
  {
    if (!orbitMap.contains(key))
      distanceSeq ++ Seq((key, distance))
    else
      getOrbitalTransfersList(orbitMap, distanceSeq ++ Seq((key, distance)), orbitMap(key), distance + 1)
  }

  def getFirstInstanceInOrbitalTransfersList(key: String, orbitalTransfersList: Seq[(String, Int)]): Seq[(String, Int)] =
  {
    if (orbitalTransfersList.isEmpty)
      Seq()
    else
    {
      if (orbitalTransfersList.head._1 == key)
        Seq(orbitalTransfersList.head)
      else
        getFirstInstanceInOrbitalTransfersList(key, orbitalTransfersList.tail)
    }
  }

  def countOrbitalTransfersToSantaCore(youOrbitalTransfersList: Seq[(String, Int)],
                                       santaOrbitalTransfersList: Seq[(String, Int)]): Int =
  {
    val crossing = getFirstInstanceInOrbitalTransfersList(youOrbitalTransfersList.head._1, santaOrbitalTransfersList)
    if (crossing.nonEmpty)
      crossing.head._2 + youOrbitalTransfersList.head._2
    else
      countOrbitalTransfersToSantaCore(youOrbitalTransfersList.tail, santaOrbitalTransfersList)
  }

  def countOrbitalTransfersToSanta(input: Seq[String]): Int =
  {
    val seqOfArrayPairs = input.map(_.split(')'))
    val seqOfTuples = seqOfArrayPairs.map { case Array(f1,f2) => (f2,f1) }
    val orbitMap = seqOfTuples.toMap

    val santaOrbitalTransfersList = getOrbitalTransfersList(orbitMap, Seq(), orbitMap("SAN"), 0)
    val youOrbitalTransfersList = getOrbitalTransfersList(orbitMap, Seq(), orbitMap("YOU"), 0)

    countOrbitalTransfersToSantaCore(youOrbitalTransfersList, santaOrbitalTransfersList)
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(countOrbitalTransfersToSanta(
        Seq("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN")) == 4)
      println("Tests ok!")
    }

    // Read input
    //
    val input = Source.fromResource("day06.txt").getLines.toSeq

    println(countOrbitalTransfersToSanta(input))
  }
}
