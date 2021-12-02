// using resource "./resources"

package day1

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.io.Source
import scala.collection.mutable.Buffer

extension (s: String)
  def input: Seq[Int] =
    val uri = getClass().getClassLoader().getResource(s).toURI()
    val path = Paths.get(uri)
    Files.readAllLines(path).asScala.map(_.toInt).toSeq

@main
def main =

  def sliding(n : Int, inputPath: String) =
    val lines = inputPath.input.toList
    lines.sliding(n).count(arr => arr(0) < arr(n - 1))
  end sliding

  val partOne = sliding(2, "input1")
  println(partOne)
  val partTwo = sliding(4, "input2")
  println(partTwo)
  
end main
