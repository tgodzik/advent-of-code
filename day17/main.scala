// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day17

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day17"))(_.mkString)

case class Point(x: Int, y: Int)

case class Target(start: Point, end: Point):

  def isContained(p: Point) =
    start.x < p.x && p.x < end.x && start.y < p.y && p.y < end.y

def parse(input: String): Target =
  input.stripPrefix("target area: ").split(raw",\s+") match
    case Array(xstring, ystring) =>
      val Array(xStart, xEnd) = xstring.stripPrefix("x=").split(raw"\.\.")
      val Array(yStart, yEnd) = ystring.stripPrefix("y=").split(raw"\.\.")
      Target(Point(xStart.toInt, yStart.toInt), Point(xEnd.toInt, yEnd.toInt))
    case _ => throw IllegalArgumentException("Invalid input")

@tailrec
def fly(currentY: Int, speed: Int, step: Int, target: Target, plausibleSteps: Set[Int]): Set[Int] =
  if target.start.y > currentY then plausibleSteps
  else if target.start.y <= currentY && currentY <= target.end.y then
    fly(currentY + speed, speed - 1, step + 1, target, plausibleSteps + step)
  else fly(currentY + speed, speed - 1, step + 1, target, plausibleSteps)

def tryY(maxY: Int, target: Target) =
  for speedY <- 0 to maxY yield fly(0, speedY, 0, target, Set.empty) match
    case plausibleSteps if plausibleSteps.nonEmpty =>
      val n = plausibleSteps.head
      val maxPossibleSpeed = target.end.x / n + (n - 1) / 2
      Some((n, maxPossibleSpeed, speedY))
    case _ =>
      None

def part1(input: String) =
  val box = parse(input)
  val (_, _, maxYSpeed) = tryY(100, box).flatten.last
  ((1 + maxYSpeed) / 2) * maxYSpeed

def tryYOptions(maxY: Int, target: Target) =
  for speedY <- -200 to maxY yield fly(0, speedY, 0, target, Set.empty) match
    case plausibleSteps if plausibleSteps.nonEmpty =>
      plausibleSteps.map { n =>
        // if speed is smaller than number of steps that means probe stopped earlier
        @tailrec
        def calculate(n: Int): (Int, Int) =
          val maxPossibleSpeed = Math.floor(target.end.x.toDouble / n + (n - 1) / 2.0).toInt
          val minPossibleSpeed = Math.ceil(target.start.x.toDouble / n + (n - 1) / 2.0).toInt
          if minPossibleSpeed < n then calculate(n - 1)
          else (minPossibleSpeed, maxPossibleSpeed)
        val (minPossibleSpeed, maxPossibleSpeed) = calculate(n)
        (minPossibleSpeed to maxPossibleSpeed).map(x => (x, speedY))
      }
    case _ =>
      Nil

def part2(input: String) =
  val box = parse(input)
  tryYOptions(200, box).flatten.flatten.toSet.size

end part2
