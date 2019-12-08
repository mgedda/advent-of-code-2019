package day08

import day08.Day08_1.getLayers

import scala.io.Source

object Day08_2
{
  def printImage(image: IndexedSeq[Int], xs: Int): Unit =
  {
    for (i <- 0 until image.size)
    {
      if (i % xs == 0) print("\n")
      print(image(i))
    }
    print("\n\n")
  }

  def getDecodedImage(imageString: String, xs: Int, ys: Int): String =
  {
    val layers = getLayers(imageString, xs, ys)
    val pixels = for {i <- 0 until (xs * ys)} yield layers.map(_(i))
    val image = pixels.map(_.find(_ != 2)).map(_.getOrElse(2))
    printImage(image, xs)
    image.map(_.toString).mkString("")
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(getDecodedImage("0222112222120000", 2, 2) == "0110")
      println("Tests ok!")
    }

    // Read input
    //
    val codedImage = Source.fromResource("day08.txt").getLines.toSeq(0)
    getDecodedImage(codedImage, 25, 6)
  }
}
