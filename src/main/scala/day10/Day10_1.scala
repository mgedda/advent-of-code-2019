package day10

import scala.io.Source

object Day10_1
{
  def numAsteroidsInLineOfSight(asteroid: (Int, Int), asteroids: Seq[(Int, Int)]): Int =
  {
    val x0 = asteroid._1
    val y0 = asteroid._2

    val angles = asteroids.map {case (x: Int, y: Int) => {
      val dx = x - x0
      val dy = y - y0

      val hyp = Math.sqrt(dx * dx + dy * dy)

      val angle =
        if (dx >= 0 && dy >= 0)
          Math.asin(dy / hyp)                 // Quadrant 1
        else if (dx < 0 && dy >= 0)
          Math.PI - Math.asin(dy / hyp)       // Quadrant 2
        else if (dx < 0 && dy < 0)
          Math.PI + Math.asin(-dy / hyp)       // Quadrant 3
        else // (dx >= 0 && dy < 0)
          2 * Math.PI - Math.asin(-dy / hyp)   // Quadrant 4

      "%.8f".format(angle).toDouble
    }}

    angles.toSet.size
  }

  def maxNumAsteroidsInLineOfSight(input: Seq[String]): Int =
  {
    val asteroids = for { y <- 0 until input.size
                          x <- 0 until input(0).length
                          if input(y)(x) == '#' } yield (x,y)

    val numberOfAsteroidsInLineOfSight: Seq[Int] = asteroids.map(a => numAsteroidsInLineOfSight(a, asteroids.diff(Seq(a))))
    numberOfAsteroidsInLineOfSight.max
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(maxNumAsteroidsInLineOfSight(Seq(
        ".#..#",
        ".....",
        "#####",
        "....#",
        "...##")) == 8)

      assert(maxNumAsteroidsInLineOfSight(Seq(
        "......#.#.",
        "#..#.#....",
        "..#######.",
        ".#.#.###..",
        ".#..#.....",
        "..#....#.#",
        "#..#....#.",
        ".##.#..###",
        "##...#..#.",
        ".#....####")) == 33)

      assert(maxNumAsteroidsInLineOfSight(Seq(
        "#.#...#.#.",
        ".###....#.",
        ".#....#...",
        "##.#.#.#.#",
        "....#.#.#.",
        ".##..###.#",
        "..#...##..",
        "..##....##",
        "......#...",
        ".####.###.")) == 35)

      assert(maxNumAsteroidsInLineOfSight(Seq(
        ".#..#..###",
        "####.###.#",
        "....###.#.",
        "..###.##.#",
        "##.##.#.#.",
        "....###..#",
        "..#.#..#.#",
        "#..#.#.###",
        ".##...##.#",
        ".....#.#..")) == 41)

      assert(maxNumAsteroidsInLineOfSight(Seq(
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
        "###.##.####.##.#..##")) == 210)

      println("Tests ok!")
    }

    // Read input
    //
    val program = Source.fromResource("day10.txt").getLines.toSeq
    println(maxNumAsteroidsInLineOfSight(program))
  }

}
