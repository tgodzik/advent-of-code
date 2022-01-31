// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day19

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
  Using.resource(Source.fromFile("input/day19"))(_.mkString)

def parse(input: String): List[Scanner] =
  input.trim
    .split("--- scanner \\d+ ---")
    .filter(_.nonEmpty)
    .zipWithIndex
    .map { case (scannerInput, index) =>
      val probes = scannerInput.trim.split("\n").map { probe =>
        probe.split(",") match
          case Array(x, y, z) => Point(x.toInt, y.toInt, z.toInt)
          case line           => throw IllegalArgumentException(probe)
      }
      Scanner(index, probes.toList)
    }
    .toList
end parse

case class Point(x: Int, y: Int, z: Int):
  def -(other: Point) = Point(x - other.x, y - other.y, z - other.z)
  def +(other: Point) = Point(x + other.x, y + other.y, z + other.z)
type Relation = Point => Point
case class Scanner(id: Int, detectedProbes: List[Point]):
  override def toString = s"[$id]"

case class RelativeScanner(relationTo0: Relation, recalculatedScanner: Scanner)
val rotations: List[Relation] =
  val tops: List[Relation] =
    List(
      p => Point(p.x, p.y, p.z),
      p => Point(p.x, p.z, -p.y),
      p => Point(p.x, -p.y, -p.z),
      p => Point(p.x, -p.z, p.y),
      p => Point(-p.z, p.y, p.x),
      p => Point(p.z, p.y, -p.x)
    )

  val rotations: List[Relation] =
    List(p => Point(p.x, p.y, p.z), p => Point(-p.y, p.x, p.z), p => Point(-p.x, -p.y, p.z), p => Point(p.y, -p.x, p.z))
  for
    rotation <- rotations
    top <- tops
  yield (p: Point) => rotation(top(p))
end rotations

def findCommon(scanner1: Scanner, scanner2: Scanner): Option[Relation] =
  val firstPoints = scanner1.detectedProbes.toSet
  val relation = scanner1.detectedProbes.iterator.flatMap { p1 =>
    scanner2.detectedProbes.iterator.flatMap { p2 =>
      rotations
        .find { rotation =>
          val relativeDist = rotation(p2) - p1
          val mapped = scanner2.detectedProbes.map(p => rotation(p) - relativeDist).toSet
          (mapped & firstPoints).size >= 12
        }
        .map { rotation =>
          val relativeDist = rotation(p2) - p1
          (p: Point) => rotation(p) - relativeDist
        }
    }

  }
  if relation.hasNext then Some(relation.next())
  else None
end findCommon

def findRelativePositions(scanners: List[Scanner]): List[(Scanner, (Scanner, Relation))] =

  val size = scanners.size.toDouble
  for
    (scanner1, index1) <- scanners.zipWithIndex
    (scanner2, index2) <- scanners.zipWithIndex
    if scanner1.id != scanner2.id
    relation <- findCommon(scanner1, scanner2).map { relation =>
      scanner1 -> (scanner2, relation)
    }
  yield
    println(100 * (index1.toDouble * size + index2.toDouble) / (size * size))
    relation

def constructGraph(
    found: Set[Int],
    currentScanners: List[RelativeScanner],
    maxSize: Int,
    relativePositions: List[(Scanner, (Scanner, Relation))]
): List[RelativeScanner] =
  if found.size == maxSize then currentScanners
  else
    val newAdded = currentScanners.flatMap { scanner =>
      relativePositions.flatMap {
        case (source, (dest, relation)) if source.id == scanner.recalculatedScanner.id && !found(dest.id) =>
          val newRelation = relation.andThen(scanner.relationTo0)
          Some(
            RelativeScanner(
              newRelation,
              Scanner(dest.id, dest.detectedProbes.map(newRelation))
            )
          )
        case _ =>
          None
      }
    }
    if newAdded.isEmpty then throw new IllegalArgumentException("")
    constructGraph(
      found ++ newAdded.map(_.recalculatedScanner.id),
      currentScanners ++ newAdded,
      maxSize,
      relativePositions
    )

def part1(input: String) =
  val scanners = parse(input)
  val relativePositions = findRelativePositions(scanners)
  println(relativePositions.map(a => a._1 -> a._2._1))
  val all = constructGraph(Set(0), List(RelativeScanner(identity, scanners.head)), scanners.size, relativePositions)
  all.flatMap(_.recalculatedScanner.detectedProbes).toSet.size

end part1

def part2(input: String) =
  throw new Exception("")
  val scanners = parse(input)
  val relativePositions = findRelativePositions(scanners)
  val singlePoint = relativePositions.map { case (scanner1, (scanner2, relation)) =>
    (
      scanner1.copy(detectedProbes = List(Point(0, 0, 0))),
      (scanner2.copy(detectedProbes = List(Point(0, 0, 0))), relation)
    )
  }
  val all = constructGraph(
    Set(0),
    List(RelativeScanner(identity, Scanner(0, List(Point(0, 0, 0))))),
    scanners.size,
    singlePoint
  )
  val scannerPositions = all.flatMap(_.recalculatedScanner.detectedProbes).toSet
  val manhattans = for 
    scanner1 <- scannerPositions
    scanner2 <- scannerPositions
  yield
    val diff = (scanner1 - scanner2)
    Math.abs(diff.x) + Math.abs(diff.y) + Math.abs(diff.z)
  manhattans.max
end part2
