// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day12

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day12"))(_.mkString)

case class Path(nodes: List[String])
case class Graph(input: String, canVisitSingleCaveTwice: Boolean):
  private val graphMap = input
    .split("\n")
    .toList
    .flatMap { line =>
      line.split("-") match
        case Array(start, end) => List(start -> end, end -> start)
        case _ => throw IllegalArgumentException(s"Unexpected input:\n$line")
    }
    .groupBy(tuple => tuple(0))
    .map { case (v, k) =>
      v -> k.map(tpl => tpl(1))
    }

  def allPaths =
    def loop(
        node: String,
        visitedSmall: Set[String],
        path: List[String],
        alreadyVisistedTwice: Boolean
    ): List[Path] =
      if node == "end" then Path(path.reverse) :: Nil
      else
        val candidates = graphMap(node).filter { candidate =>
          if candidate != "start" && !alreadyVisistedTwice then true
          else !visitedSmall(candidate)
        }
        val updatedSmall =
          if node.head.isLower then visitedSmall + node else visitedSmall
        candidates.flatMap { candidate =>
          val updatedAlreadyVisistedTwice =
            alreadyVisistedTwice || visitedSmall(candidate)
          loop(
            candidate,
            updatedSmall,
            candidate :: path,
            updatedAlreadyVisistedTwice
          )
        }
    loop("start", Set.empty, Nil, !canVisitSingleCaveTwice)
  end allPaths
end Graph

def part1(input: String) =
  val graph = Graph(input, canVisitSingleCaveTwice = false)
  graph.allPaths.size

def part2(input: String) =
  val graph = Graph(input, canVisitSingleCaveTwice = true)
  graph.allPaths.size
end part2
