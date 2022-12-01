// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6
// using javaOpt -Xss128m

package day15

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day15"))(_.mkString)

def parse(input: String): CaveMap =
  val lines = input.split("\n")
  val size = lines.size
  val allPoints = for
    (line, y) <- lines.zipWithIndex
    (char, x) <- line.zipWithIndex
  yield Point(x, y) -> char.toString.toInt
  CaveMap(allPoints.toMap, lines.head.size, size)

case class Point(x: Int, y: Int)
case class Path(nodes: List[Point], cost: Int)

case class CaveMap(inputMap: Map[Point, Int], sizeX: Int, sizeY: Int):
  object PointOrdering extends Ordering[Point]:
    def compare(a: Point, b: Point) = inputMap(a) compare inputMap(b)

  def repeatTile: CaveMap =
    val updated = inputMap.flatMap { case (point, value) =>
      for
        i <- 0 until 5
        j <- 0 until 5
      yield
        val updated = value + i + j
        point.copy(
          x = point.x + i * sizeX,
          y = point.y + j * sizeY
        ) -> (updated % 10 + updated / 10)
    }
    CaveMap(updated, sizeX * 5, sizeY * 5)

  def printMap =
    for y <- 0 until sizeY do
      for x <- 0 until sizeX do print(inputMap(Point(x, y)))
      print("\n")

  def dynamicPaths =
    val costsMap = mutable.Map(Point(0, 0) -> 0)

    def printCosts =
      for y <- 0 until sizeY do
        for x <- 0 until sizeX do
          print(costsMap.getOrElse(Point(x, y), -1) + " | ")
        print("\n")

    def iterateUp(p: Point): Int =
      if !inputMap.contains(p) then Int.MaxValue
      else
        val upValue = iterateUp(p.copy(y = p.y - 1))
        val leftValue = costsMap(p.copy(x = p.x - 1))
        val min = Math.min(upValue, leftValue) + inputMap(p)
        costsMap(p) = min
        min

    @tailrec
    def iterate(pointsToVisit: Set[Point], visited: Set[Point]): Unit =
      if pointsToVisit.nonEmpty then
        val point =
          pointsToVisit.minBy(p => costsMap.getOrElse(p, Int.MaxValue))
        val value = costsMap(point)
        val neighbourhood =
          Seq(
            Point(point.x - 1, point.y),
            Point(point.x + 1, point.y),
            Point(point.x, point.y - 1),
            Point(point.x, point.y + 1)
          )
        val newPoints = neighbourhood.flatMap { nPoint =>
          if inputMap.contains(nPoint) then
            val currentValue = costsMap.getOrElse(nPoint, Int.MaxValue)
            val newValue = value + inputMap(nPoint)
            if currentValue > newValue then
              costsMap(nPoint) = newValue
              if !visited(nPoint) then Some(nPoint)
              else None
            else None
          else None
        }
        iterate((pointsToVisit - point) ++ newPoints, visited + point)

    costsMap(Point(0, 0)) = 0
    iterate(Set(Point(0, 0)), Set.empty)
    costsMap(Point(sizeX - 1, sizeY - 1))
  end dynamicPaths
end CaveMap

def part1(input: String) =
  val cave = parse(input)
  cave.dynamicPaths

def part2(input: String) =
  val cave = parse(input).repeatTile
  cave.dynamicPaths
end part2
