// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day13

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is:\n${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day13"))(_.mkString)

def parse(input: String) =
  val lines = input.split("\n")
  val points = lines.takeWhile(_.nonEmpty).map { line =>
    line.split(",") match
      case Array(x, y) => Point(x.toInt, y.toInt)
      case _ => throw java.lang.IllegalArgumentException(s"Invalid line: $line")
  }
  val foldLines =
    lines.dropWhile(line => !line.startsWith("fold along")).map { line =>
      line.stripPrefix("fold along").trim.split("=") match
        case Array("x", num) => LineX(num.toInt)
        case Array("y", num) => LineY(num.toInt)
        case _ =>
          throw java.lang.IllegalArgumentException(s"Invalid line: $line")
    }
  (Sheet(points.toSet), foldLines)
end parse

def part1(input: String) =
  val (sheet, foldLines) = parse(input)
  sheet.fold(foldLines.head).points.size
end part1

case class Point(x: Int, y: Int)

trait Line
case class LineX(x: Int) extends Line
case class LineY(y: Int) extends Line

case class Sheet(points: Set[Point]):

  def fold(line: Line): Sheet =
    val updated = points.map { case point @ Point(x, y) =>
      line match
        case LineX(foldX) if x > foldX =>
          Point(foldX - (x - foldX), y)
        case LineY(foldY) if y > foldY =>
          Point(x, foldY - (y - foldY))
        case _ =>
          point
    }
    this.copy(points = updated)

  def show =
    val sizeX = points.maxBy(_.x).x
    val sizeY = points.maxBy(_.y).y
    val builder = new mutable.StringBuilder
    for y <- 0 to sizeY do
      for x <- 0 to sizeX do
        if points(Point(x, y)) then builder.append("#") else builder.append(".")
      builder.append("\n")
    builder.toString
end Sheet

def part2(input: String) =
  val (sheet, foldLines) = parse(input)
  val folded = foldLines.foldLeft(sheet) { case (sheet, foldLine) =>
    sheet.fold(foldLine)
  }
  folded.show
end part2
