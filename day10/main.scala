// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day10

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day9"))(_.mkString)

sealed trait ParseOutput

case class Invalid(errorChar: Char) extends ParseOutput
case class Incomplete(missing: List[Char]) extends ParseOutput

def findInvalid(line: String) =
  @tailrec
  def loop(current: List[Char], expectedEnds: List[Char]): Option[ParseOutput] =
    current match
      case Nil         => Some(Incomplete(expectedEnds))
      case '{' :: tail => loop(tail, '}' :: expectedEnds)
      case '[' :: tail => loop(tail, ']' :: expectedEnds)
      case '<' :: tail => loop(tail, '>' :: expectedEnds)
      case '(' :: tail => loop(tail, ')' :: expectedEnds)
      case end :: tail =>
        expectedEnds match
          case head :: leftover if head == end => loop(tail, leftover)
          case _                               => Some(Invalid(end))
  loop(line.toList, Nil)
end findInvalid

def part1(input: String) =
  val errorScores = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val lines = input.split("\n")
  val scores =
    for line <- lines
    yield findInvalid(line) match
      case Some(Invalid(end)) => errorScores.getOrElse(end, 0)
      case _                  => 0

  scores.sum

def part2(input: String) =
  val errorScores = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  val lines = input.split("\n")
  val scores =
    for line <- lines
    yield findInvalid(line) match
      case Some(Incomplete(missing)) =>
        missing.foldLeft(0l) { (sum, curr) => sum * 5 + errorScores(curr) }
      case _ => 0
  val results = scores.filter(_ != 0).sorted
  results(results.size / 2)
