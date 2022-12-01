// using resource "./resources"
// using lib com.lihaoyi::pprint:0.6.6

package day5

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.collection.mutable

case class Point(x: Int, y: Int)

object Point:
  def apply(str: String) =
    str.split(",") match
      case Array(start, end) => new Point(start.trim.toInt, end.trim.toInt)
      case _ =>
        throw new java.lang.IllegalArgumentException(s"Wrong point input $str")

case class Vent(start: Point, end: Point)

object Vent:
  def apply(str: String) =
    str.split("->") match
      case Array(start, end) =>
        new Vent(Point(start), Point(end))
      case _ =>
        throw new java.lang.IllegalArgumentException(s"Wrong vent input $str")

extension (s: String)
  def input: Seq[Vent] =
    val uri = getClass().getClassLoader().getResource(s).toURI()
    val path = Paths.get(uri)
    for line <- Files.readAllLines(path).asScala.toSeq
    yield Vent(line)

def findDangerousPoints(vents: Seq[Vent]) =
  val map = mutable.Map[Point, Int]().withDefaultValue(0)
  def update(p: Point) =
    val current = map(p)
    map.update(p, current + 1)

  for vent <- vents do
    def rangex =
      val stepx = if vent.end.x > vent.start.x then 1 else -1
      vent.start.x.to(vent.end.x, stepx)
    def rangey =
      val stepy = if vent.end.y > vent.start.y then 1 else -1
      vent.start.y.to(vent.end.y, stepy)
    if vent.start.x == vent.end.x then
      for py <- rangey do update(Point(vent.start.x, py))
    else if vent.start.y == vent.end.y then
      for px <- rangex do update(Point(px, vent.start.y))
    else for (px, py) <- rangex.zip(rangey) do update(Point(px, py))
  end for

  map.count { case (_, v) => v > 1 }
end findDangerousPoints

def part1() =
  val onlySimple =
    "input1".input.filter(v => v.start.x == v.end.x || v.start.y == v.end.y)
  val dangerousPoints = findDangerousPoints(onlySimple)
  pprint.log(dangerousPoints)
end part1

def part2() =
  val allVents = "input1".input
  val dangerousPoints = findDangerousPoints(allVents)
  pprint.log(dangerousPoints)
end part2

@main
def main =
  part1()
  part2()

end main
