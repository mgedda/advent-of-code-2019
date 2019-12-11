package day10

import scala.io.Source

object Day10_2
{
  def getAngles(asteroid: (Int, Int), asteroids: Seq[(Int, Int)]): Seq[Double] =
  {
    val x0 = asteroid._1
    val y0 = asteroid._2

    asteroids.map {case (x: Int, y: Int) => {
      val dx = x - x0
      val dy = y - y0

      val hyp = Math.sqrt(dx * dx + dy * dy)

      val angle =
        if (dx >= 0 && dy >= 0)
          Math.PI/2.0 + Math.asin(dy / hyp)                  // Quadrant 4
        else if (dx < 0 && dy >= 0)
          Math.PI + (Math.PI/2.0 - Math.asin(dy / hyp))      // Quadrant 3
        else if (dx < 0 && dy < 0)
          2*Math.PI - (Math.PI/2.0 - Math.asin(-dy / hyp))   // Quadrant 2
        else // (dx >= 0 && dy < 0)
          Math.PI/2.0 - Math.asin(-dy / hyp)                 // Quadrant 1

      "%.8f".format(angle).toDouble
    }}
  }

  def getNumAsteroidsInLineOfSight(asteroid: (Int, Int), asteroids: Seq[(Int, Int)]): Int =
  {
    getAngles(asteroid, asteroids).toSet.size
  }

  def findBestAsteroid(asteroids: Seq[(Int, Int)]): (Int, Int) =
  {
    val numberOfAsteroidsInLineOfSights = asteroids.map(a => getNumAsteroidsInLineOfSight(a, asteroids.diff(Seq(a))))
    val bestAsteroidIndex = numberOfAsteroidsInLineOfSights.indexOf(numberOfAsteroidsInLineOfSights.max)
    asteroids(bestAsteroidIndex)
  }

  def getClosestAsteroid(asteroid: (Int, Int), asteroids: Seq[(Int, Int)]): (Int, Int) =
  {
    asteroids.minBy(a => Math.sqrt((a._1 - asteroid._1)*(a._1 - asteroid._1) + (a._2 - asteroid._2)*(a._2 - asteroid._2)))
  }

  def vaporizeAsteroid(station: (Int, Int),
                       angleToAsteroidsMap: Map[Double, Seq[(Int, Int)]],
                       angles: Seq[Double],
                       angleIndex: Int,
                       numAsteroidsVaporized: Int,
                       stopValue: Int): (Int, Int) =
  {
    if (numAsteroidsVaporized == stopValue - 1)
    {
      // return next asteroid
      val asteroidsInAngle = angleToAsteroidsMap(angles(angleIndex))
      if (asteroidsInAngle.isEmpty)
        vaporizeAsteroid(station, angleToAsteroidsMap, angles, (angleIndex + 1) % angles.size, numAsteroidsVaporized, stopValue)
      else
        getClosestAsteroid(station, asteroidsInAngle)
    }
    else
    {
      // Vaporize asteroid
      val angle = angles(angleIndex)
      val asteroidsInAngle = angleToAsteroidsMap(angle)

      if (asteroidsInAngle.isEmpty)
        vaporizeAsteroid(station, angleToAsteroidsMap, angles, (angleIndex + 1) % angles.size, numAsteroidsVaporized, stopValue)
      else
      {
        val mapWithAngleRemoved = angleToAsteroidsMap - angle
        val closestAsteroid = getClosestAsteroid(station, asteroidsInAngle)
        val mapWithAngleReinserted = mapWithAngleRemoved + (angle -> asteroidsInAngle.diff(Seq(closestAsteroid)))

        {
          val number = numAsteroidsVaporized + 1
          val x = closestAsteroid._1
          val y = closestAsteroid._2
          println(f"The $number asteroid to be vaporized is at $x,$y")
        }

        vaporizeAsteroid(station, mapWithAngleReinserted, angles, (angleIndex + 1) % angles.size, numAsteroidsVaporized + 1, stopValue)
      }
    }
  }

  def vaporizationCoordinateValue(input: Seq[String], vaporizationNumber: Int): Int =
  {
    val asteroids = for { y <- 0 until input.size
                          x <- 0 until input(0).length
                          if input(y)(x) == '#' } yield (x,y)

    val bestAsteroid = findBestAsteroid(asteroids)

    {
      val x = bestAsteroid._1
      val y = bestAsteroid._2
      println(f"The monitoring station is located at $x,$y")
    }

    val otherAsteroids = asteroids.diff(Seq(bestAsteroid))
    val anglesUnsorted = getAngles(bestAsteroid, otherAsteroids)
    val angleAsteroids = anglesUnsorted.zip(otherAsteroids)
    val angleMap = angleAsteroids.groupBy(_._1).map(p => p._1 -> p._2.map(_._2))
    val anglesSorted = anglesUnsorted.toSet.toSeq.sorted
    val nextAsteroid = vaporizeAsteroid(bestAsteroid, angleMap, anglesSorted, 0, 0, vaporizationNumber)
    nextAsteroid._1 * 100 + nextAsteroid._2
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(vaporizationCoordinateValue(Seq(
        ".#..##.###...#######",
        "##.############..##.",
        ".#.######.########.#",
        ".###.#######.####.#.",
        "#####.##.#.##.###.##",
        "..#####..#.#########",
        "####################",
        "#.####....###.#.#.##",
        "##.#################",
        "#####.##.###..####..",
        "..######..##.#######",
        "####.##.####...##..#",
        ".#####..#.######.###",
        "##...#.##########...",
        "#.##########.#######",
        ".####.#.###.###.#.##",
        "....##.##.###..#####",
        ".#.#.###########.###",
        "#.#.#.#####.####.###",
        "###.##.####.##.#..##"), 299) == 1101)

      println("Tests ok!")
    }


    // Read input
    //
    val program = Source.fromResource("day10.txt").getLines.toSeq
    println(vaporizationCoordinateValue(program, 200))
  }

}
