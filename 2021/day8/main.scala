// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day7

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day8example"))(_.mkString)

def part1(input: String) =
  val uniqueNumbers = Set(2, 3, 4, 7)
  val outputValues =
    input.split("\n").flatMap(_.split("\\|").last.trim.split("\\s+"))
  outputValues.map { letters =>
    if uniqueNumbers(letters.length) then 1
    else 0
  }.sum

case class Digit(chars: Set[Char])

case class Display(signal: Seq[Digit], output: Seq[Digit]):

  def decodedOutput =
    val one = signal.find(_.chars.size == 2).get
    val three =
      signal.find(d => d.chars.size == 5 && (d.chars & one.chars).size == 2).get
    val four = signal.find(_.chars.size == 4).get
    val two =
      signal.find(d => d.chars.size == 5 && (d.chars & four.chars).size == 2).get
    val five = signal.find(d =>
      d.chars.size == 5 && (d.chars & four.chars).size == 3 && d != three
    ).get
    val seven = signal.find(_.chars.size == 3).get
    val eight = signal.find(_.chars.size == 7).get
    val nine = signal
      .find(d => d.chars.size == 6 && (d.chars & four.chars).size == 4)
      .get
    val zero =
      signal
        .find(d =>
          d.chars.size == 6 && (d.chars & one.chars).size == 2 && d != nine
        )
        .get
    val six =
      signal.find(d => d.chars.size == 6 && (d.chars & one.chars).size == 1).get
    val mapping = Seq(zero, one, two, three, four, five, six, seven, eight, nine).zipWithIndex.toMap

    val outputNum = output.map{
      case d => mapping(d).toString
    }.mkString

    outputNum.toInt
  end decodedOutput
end Display

def part2(input: String) =
  val displays =
    input.split("\n").map { line =>
      line.split("\\|") match
        case Array(signal, output) =>
          Display(
            signal.split("\\s").toSeq.map(s => Digit(s.toSet)),
            output.trim.split("\\s").toSeq.map(s => Digit(s.toSet))
          )
        case _ =>
          throw new java.lang.IllegalArgumentException(
            s"Unexpected line:\n$line"
          )
    }

  val nums = for display <- displays yield display.decodedOutput
  nums.sum
end part2
