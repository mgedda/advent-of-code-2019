package day04

import scala.collection.mutable
import scala.io.Source

object Day04_2
{
  def isValidPassword(number: Int): Boolean =
  {
    val digits = number.toString.map(_.toInt)

    val multiples = mutable.HashMap[Int, Int]().withDefaultValue(0)

    for (i <- 1 until digits.size)
    {
      if (digits(i) < digits(i-1))
      {
        return false
      }
      if (digits(i) == digits(i-1))
      {
        multiples.put(digits(i), multiples(digits(i)) + 1)
      }
    }

    multiples.values.exists(v => v == 1)
  }

  def numValidPasswords(input: String): Int =
  {
    val min = input.split('-')(0).toInt
    val max = input.split('-')(1).toInt

    (min to max).count(number => isValidPassword(number))
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(numValidPasswords("210-240") == 8)
      println("Tests ok!")
    }

    // Read input
    //
    val input = Source.fromResource("day04.txt").getLines.toSeq(0)

    println(numValidPasswords(input))
  }
}
