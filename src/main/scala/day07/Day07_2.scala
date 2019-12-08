package day07

import day07.Status.Exit

import scala.io.Source

object Day07_2
{
  def getThrusterSignal(instructions: Seq[Int], phases: Seq[Int]): Int =
  {
    // Create amps
    val amps = phases.zip(Seq("A", "B", "C", "D", "E")).map { case (phase, letter) => new Amp(instructions, letter, phase)}

    // Initial input to first amp
    var outputs = Seq(0)

    var i = 0      // counter
    while (true)   // runt until last amp (E) exits with 99
    {
      // Run amp until it halts
      val halt = amps(i).run(outputs)

      // Check why amp halted
      halt match {
        case Halt(Exit,_,_) =>
          outputs = halt.outputs            // store output from amp
          if (amps(i).letter == "E")
            return outputs.head             // last amp exited with 99
        case _ => outputs = halt.outputs    // store output from amp
      }

      // increase counter
      i = (i + 1) % amps.size
    }

    -1
  }

  def findMaxThrusterSignalCore(instructions: Seq[Int], phasePermutations: Seq[Seq[Int]], maxThrusterSignal: Int): Int =
  {
    if (phasePermutations.isEmpty)
      maxThrusterSignal
    else
    {
      // Compute thruster signal for this specific phase permutation
      val thrusterSignal = getThrusterSignal(instructions, phasePermutations.head)

      // Keep max thruster signal and compute thruster signal for next permutation
      findMaxThrusterSignalCore(instructions, phasePermutations.tail, Math.max(thrusterSignal, maxThrusterSignal))
    }
  }

  def findMaxThrusterSignal(input: String): Int =
  {
    val instructions = input.split(',').toSeq.map(_.toInt)
    val phasePermutations = Seq(5,6,7,8,9).permutations.toSeq

    findMaxThrusterSignalCore(instructions, phasePermutations, Int.MinValue)
  }

  def main(args: Array[String]): Unit =
  {
    //-- Tests -----------------
    {
      assert(findMaxThrusterSignal("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,"
        + "-1,28,1005,28,6,99,0,0,5") == 139629729)
      assert(findMaxThrusterSignal("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,"
        + "54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,"
        + "56,6,99,0,0,0,0,10") == 18216)
      println("Tests ok!")
    }

    // Read input
    //
    val program = Source.fromResource("day07.txt").getLines.toSeq(0)

    println(findMaxThrusterSignal(program))
  }
}
