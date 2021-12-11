// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6

package day11

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable
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
  private val currentSituation = mutable.Map[Point, Int](inputMap.toSeq*)
  private val points = currentSituation.keys.toList

  private def increment(p: Point): Unit =
    if (currentSituation.contains(p)) then
      currentSituation(p) = currentSituation(p) + 1
      currentSituation(p)

  private def propagate(
      toVisit: Queue[Point],
      alreadyFlashed: Set[Point]
  ): Unit =
    toVisit.dequeueOption match
      case None =>
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
            propagated.foreach(increment)
            propagate(dequeued.appendedAll(propagated), alreadyFlashed + point)
          case _ =>
            propagate(dequeued, alreadyFlashed)
  end propagate

  def simulate(step: Step): Step =
    if step.shouldStop then step
    else
      points.foreach { p =>
        currentSituation(p) = currentSituation(p) + 1
      }
      val flashes = points.collect {
        case p if currentSituation(p) > 9 => p
      }.toList
      propagate(Queue(flashes*), Set.empty)
      val newFlashes = points.map {
        case p if currentSituation(p) >= 10 =>
          currentSituation(p) = 0
          1
        case _ =>
          0
      }.sum
      simulate(step.increment.addFlashes(newFlashes))
  end simulate

end Octopei

def part1(input: String) =
  val octopei = parse(input)
  octopei.simulate(MaxIterStep(0, 0, 100)).currentFlashes

def part2(input: String) =
  val octopei = parse(input)
  octopei
    .simulate(SynchronizationStep(0, 0, octopei.inputMap.size, 0))
    .n
