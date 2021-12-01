package day1

import utils.Inputs.*

@main
def main =

  def sliding(n : Int, inputPath: String) =
    val lines = inputPath.input.map(_.toInt).toList
    lines.sliding(n).count(arr => arr(0) < arr(n - 1))
  end sliding

  val partOne = sliding(2, "day1/input1")
  println(partOne)
  val partTwo = sliding(4, "day1/input2")
  println(partTwo)
  
end main
