// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6
// using javaOpt -Xss128m

package day20

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
  Using.resource(Source.fromFile("input/day20"))(_.mkString)

case class Point(x: Int, y: Int)
case class Image(map: Map[Point, Char]):

  val (Point(minx, _), _) = map.minBy { case (p, _) => p.x }
  val (Point(maxx, _), _) = map.maxBy { case (p, _) => p.x }
  val (Point(_, miny), _) = map.minBy { case (p, _) => p.y }
  val (Point(_, maxy), _) = map.maxBy { case (p, _) => p.y }
  override def toString() =
    val buffer = new StringBuffer()
    for y <- miny to maxy do
      for x <- minx to maxx do buffer.append(map.getOrElse(Point(x, y), "."))
      buffer.append("\n")
    buffer.toString

case class ImageEnhancementAlgorithm(enhancement: Array[Char], image: Image, default: Char):
  override def toString() =
    enhancement.sliding(74, 74).map(_.mkString).mkString("\n") + "\n\n" + image

  def enhancePoint(point: Point) =
    val all = for
      y <- (point.y - 1) to (point.y + 1)
      x <- (point.x - 1) to (point.x + 1)
    yield image.map.getOrElse(Point(x, y), default)
    val binaryString = all.map {
      case '#' => '1'
      case '.' => '0'
    }.mkString
    point -> enhancement(Integer.parseInt(binaryString, 2))

  def enhance() =
    val newPoints = for
      y <- (image.miny - 1) to (image.maxy + 1)
      x <- (image.minx - 1) to (image.maxx + 1)
    yield enhancePoint(Point(x, y))
    val defaultBin = if (default == '.') 0 else 1
    this.copy(image = Image(newPoints.toMap), default = enhancement(Integer.parseInt(defaultBin.toString * 9, 2)))
  def litPixels =
    image.map.count { case (_, ch) => ch == '#' }
end ImageEnhancementAlgorithm

def parse(input: String): ImageEnhancementAlgorithm =
  val Array(enhancementString, imageString) = input.split("\n\n")

  val enhancement = enhancementString.replace("\n", "").toArray
  val lines = imageString.split("\n")
  val allPixels = for
    (line, y) <- lines.zipWithIndex
    (pixel, x) <- line.zipWithIndex
  yield Point(x, y) -> pixel

  val image = Image(allPixels.toMap)
  ImageEnhancementAlgorithm(enhancement, image, '.')

def part1(input: String) =
  val algo = parse(input)
  println(algo)
  val enhanced1 = algo.enhance()
  println(enhanced1)
  val enhanced2 = enhanced1.enhance()
  println(enhanced2)
  enhanced2.litPixels

def part2(input: String) =
  val algo = parse(input)
  val result = (0 until 50).foldLeft(algo){
    case (prev, _) => prev.enhance()
  }
  println(result)
  result.litPixels
end part2
