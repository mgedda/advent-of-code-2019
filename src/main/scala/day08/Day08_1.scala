package day08

import scala.io.Source

object Day08_1
{
  def getLayers(imageString: String, xs: Int, ys: Int): Seq[Array[Int]] =
  {
    val image = imageString.toSeq.map(_.toInt - 48).toArray
    val imageSize = xs * ys
    val numLayers = image.length / imageSize
    for {i <- 0 until numLayers} yield image.slice(0 + i * imageSize, imageSize + i * imageSize)
  }

  def findImageValue(imageString: String, xs: Int, ys: Int): Int =
  {
    val layers = getLayers(imageString, xs, ys)

    val zeroDigits = layers.map(l => l.count(_ == 0))
    val index = zeroDigits.indexOf(zeroDigits.min)
    layers(index).count(_ == 1) * layers(index).count(_ == 2)
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findImageValue("123456789012", 3, 2) == 1)
      println("Tests ok!")
    }

    // Read input
    //
    val codedImage = Source.fromResource("day08.txt").getLines.toSeq(0)

    println(findImageValue(codedImage, 25, 6))
  }
}
