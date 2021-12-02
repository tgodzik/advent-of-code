// using resource "./resources"

package day1

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.io.Source

extension (s: String)
  def input =
    Source.fromFile(s"./resources/$s").getLines.map(_.toInt)

@main
def main =

  def sliding(n : Int, inputPath: String) =
    val lines = inputPath.input
    lines.sliding(n).count(arr => arr(0) < arr(n - 1))
  end sliding

  val partOne = sliding(2, "input1")
  println(partOne)
  val partTwo = sliding(4, "input2")
  println(partTwo)
  
end main
