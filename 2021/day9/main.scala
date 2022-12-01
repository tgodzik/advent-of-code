// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day7

import scala.util.Using
import scala.io.Source
import scala.collection.mutable

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day9"))(_.mkString)

case class Point(x: Int, y: Int)
case class FloorMap(map: Map[Point, Int]):

  def isLow(p: Point) =
    val v = map(p)
    map.getOrElse(Point(p.x, p.y - 1), Int.MaxValue) > v &&
    map.getOrElse(Point(p.x, p.y + 1), Int.MaxValue) > v &&
    map.getOrElse(Point(p.x - 1, p.y), Int.MaxValue) > v &&
    map.getOrElse(Point(p.x + 1, p.y), Int.MaxValue) > v

  def dangerPoints: Map[Point, Int] =
    map.filter { case (p, v) =>
      isLow(p)
    }

  def dangerScore: Int =
    dangerPoints.values.map(_ + 1).sum

  def findBasin(point: Point): Int =

    val visited: mutable.Set[Point] = mutable.Set.empty
    def go(point: Point): Int =
      map.get(point) match
        case None                      => 0
        case Some(9)                   => 0
        case Some(v) if visited(point) => 0
        case Some(v) =>
          visited += point
          1 + go(point.copy(x = point.x + 1)) +
            go(point.copy(x = point.x - 1)) +
            go(point.copy(y = point.y + 1)) +
            go(point.copy(y = point.y - 1))
    go(point)
  end findBasin

  def basins =
    val allBasins = for lowPoint <- dangerPoints.keys.toList yield findBasin(lowPoint)
    allBasins.foldLeft(List.empty[Int]){
      case (found, cur) =>
        if(found.size < 3) cur :: found
        else 
          val minimal = found.min
          if (cur > minimal) 
            val i = found.indexWhere(e => e == minimal)
            found.updated(i, cur)
          else
            found
    }.reduce(_ * _)
end FloorMap

def parse(input: String): FloorMap =
  val lines = input.split("\n")
  val size = lines.size

  val allPoints = for
    (line, y) <- lines.zipWithIndex
    (char, x) <- line.zipWithIndex
  yield Point(x, y) -> char.toString.toInt
  FloorMap(allPoints.toMap)

def part1(input: String) =
  val floor = parse(input)
  floor.dangerScore

def part2(input: String) =
  val floor = parse(input)
  floor.basins
