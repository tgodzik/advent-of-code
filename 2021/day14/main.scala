// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6
// using lib org.scala-lang.modules::scala-parallel-collections:1.0.4

package day14

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.compiletime.ops.string
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.CollectionConverters.*

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day14"))(_.mkString)

case class Polymer(string: List[Char], mappings: Map[String, Char]):

  private val frequencies = string
    .sliding(2)
    .map(l => s"${l(0)}${l(1)}")
    .toList
    .groupBy(identity)
    .mapValues(_.size.toLong)
    .toMap

  @tailrec
  final def polimerize(
      n: Int,
      current: Map[String, Long] = frequencies
  ): Map[String, Long] =
    if n == 0 then current
    else
      val updated = current.toList
        .flatMap { case (pair, freqs) =>
          val insert = mappings(pair)
          List((s"${pair.head}$insert", freqs), (s"$insert${pair.last}", freqs))
        }
        .groupBy(_._1)
        .map { case (pair, list) =>
          pair -> list.map(_._2).sum
        }
      polimerize(n - 1, updated)

  def count(n: Int) =
    val frequencies = polimerize(n)
    val sizes = frequencies.toList
      .flatMap { case (pair, freq) =>
        List(pair.head -> freq, pair.last -> freq)
      }
      .groupBy(_._1)
      .map { case (ch, freqs) =>
        val sum = freqs.map(_._2).sum
        if ch == string.head || ch == string.last then sum / 2 + 1
        else sum / 2
      }
    sizes.max - sizes.min
  end count
end Polymer

def parse(input: String): Polymer =
  val lines = input.split("\n")
  val initialString = lines.head
  val mappings = lines
    .dropWhile(l => !l.contains("->"))
    .map { line =>
      line.split("->") match
        case Array(from, to) => from.trim -> to.trim.head
        case _               => throw java.lang.IllegalArgumentException(line)
    }
    .toMap
  Polymer(initialString.toList, mappings)

def part1(input: String) =
  val polymer = parse(input)
  polymer.count(10)

def part2(input: String) =
  val polymer = parse(input)
  polymer.count(40)
end part2

// diff 4, 7 , 12, 25, 47, 78, 144, 276, 555, 1235
// B 2, 6, 11, 22, 39, 79, 139, 258
