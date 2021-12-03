// using resource "./resources"

package day3

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.io.Source
import scala.collection.mutable.Buffer

extension (s: String)
  def input: Seq[String] =
    val uri = getClass().getClassLoader().getResource(s).toURI()
    val path = Paths.get(uri)
    Files.readAllLines(path).asScala.toList

@main
def main =

  def part1 =
    val binaryLists = "input1".input.map(_.toList)
    val size = binaryLists.head.size
    val sums = binaryLists.foldLeft(List.fill(size)(0)) { case (sums, next) =>
      sums.zip(next).map { case (sum, c) =>
        if c == '0' then sum - 1
        else sum + 1
      }
    }

    val gammaString = sums.map { s =>
      if s > 0 then "1"
      else "0"
    }.mkString

    val gamma = Integer.parseInt(gammaString, 2)
    val allOnes = (1 << size) - 1 // 111111111111
    val epsilon = gamma ^ allOnes

    epsilon * gamma
  end part1

  def part2 =
    def loop(numbers: Seq[Int], digit: Int, keepFewer: Boolean): Int =
      if digit < 0 then
        throw new Exception(
          "There are more than one numbers that fits the criteria"
        )
      val position = 1 << digit
      val ones = numbers.foldLeft(0) { case (sum, n) =>
        if (n & position) > 0 then sum + 1
        else sum - 1
      }
      val searchForOnes = if ones >= 0 then !keepFewer else keepFewer
      val resulting = numbers.filter { n =>
        if searchForOnes then (n & position) > 0
        else (n & position) == 0

      }
      resulting match
        case head :: Nil => head
        case more        => loop(more, digit - 1, keepFewer)
    end loop

    val inputStrings = "input1".input
    val size = inputStrings.head.size
    val numbers = inputStrings.map(Integer.parseInt(_, 2))
    val oxygen = loop(numbers, size - 1, keepFewer = false)
    val scrubber = loop(numbers, size - 1, keepFewer = true)
    oxygen * scrubber

  end part2

  println("Part 1:")
  println(part1)
  
  println("Part 2:")
  println(part2)
end main
