// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day11

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.Queue

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day11"))(_.mkString)

def parse(input: String): Octopei =
  val lines = input.split("\n")
  val size = lines.size
  val allPoints = for
    (line, y) <- lines.zipWithIndex
    (char, x) <- line.zipWithIndex
  yield Point(x, y) -> char.toString.toInt
  Octopei(allPoints.toMap)

trait Step:
  def increment: Step
  def addFlashes(f: Int): Step
  def shouldStop: Boolean
  def currentFlashes: Int
  def n: Int

case class MaxIterStep(currentFlashes: Int, n: Int, max: Int) extends Step:
  def increment = this.copy(n = n + 1)
  def addFlashes(f: Int) = this.copy(currentFlashes = currentFlashes + f)
  def shouldStop = n == max

case class SynchronizationStep(
    currentFlashes: Int,
    n: Int,
    maxChange: Int,
    lastFlashes: Int
) extends Step:
  def increment = this.copy(n = n + 1)
  def addFlashes(f: Int) =
    this.copy(currentFlashes = currentFlashes + f, lastFlashes = currentFlashes)
  def shouldStop = currentFlashes - lastFlashes == maxChange

case class Point(x: Int, y: Int)
case class Octopei(inputMap: Map[Point, Int]):

  @tailrec
  private def propagate(
      toVisit: Queue[Point],
      alreadyFlashed: Set[Point],
      currentSituation: Map[Point, Int]
  ): Map[Point, Int] =
    toVisit.dequeueOption match
      case None => currentSituation
      case Some((point, dequeued)) =>
        currentSituation.get(point) match
          case Some(v) if v > 9 && !alreadyFlashed(point) =>
            val propagated =
              Seq(
                point.copy(x = point.x + 1),
                point.copy(x = point.x - 1),
                point.copy(y = point.y + 1),
                point.copy(y = point.y - 1),
                point.copy(x = point.x + 1, y = point.y + 1),
                point.copy(x = point.x + 1, y = point.y - 1),
                point.copy(x = point.x - 1, y = point.y + 1),
                point.copy(x = point.x - 1, y = point.y - 1)
              )
            val newSituation = propagated.foldLeft(currentSituation) {
              case (map, point) =>
                map.get(point) match
                  case Some(v) => map.updated(point, v + 1)
                  case _       => map
            }
            propagate(
              dequeued.appendedAll(propagated),
              alreadyFlashed + point,
              newSituation
            )
          case _ =>
            propagate(dequeued, alreadyFlashed, currentSituation)
  end propagate

  def simulate(step: Step) = simulateIter(step, inputMap)

  @tailrec
  private def simulateIter(
      step: Step,
      currentSituation: Map[Point, Int]
  ): Step =
    if step.shouldStop then step
    else
      val incremented = currentSituation.map { case (p, v) =>
        (p, v + 1)
      }
      val flashes = incremented.collect {
        case (p, v) if v > 9 => p
      }.toList
      val propagated = propagate(Queue(flashes*), Set.empty, incremented)
      val newFlashes = propagated.collect {
        case (p, v) if v > 9 => 1
      }.sum
      val zeroed = propagated.map {
        case (p, v) if v > 9 => (p, 0)
        case same            => same
      }
      simulateIter(step.increment.addFlashes(newFlashes), zeroed)
  end simulateIter

end Octopei

def part1(input: String) =
  val octopei = parse(input)
  octopei.simulate(MaxIterStep(0, 0, 100)).currentFlashes

def part2(input: String) =
  val octopei = parse(input)
  octopei
    .simulate(SynchronizationStep(0, 0, octopei.inputMap.size, 0))
    .n
