// using resource "./resources"
// using lib com.lihaoyi::os-lib:0.7.8

package day1

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.io.Source
import scala.collection.mutable.Buffer

enum Direction:
  case Forward
  case Down
  case Up

case class Submarine(horizontal: Int, vertical: Int, aim: Int):
  def moveForward(value: Int) = this.copy(horizontal = horizontal + value)
  def moveUp(value: Int) = this.copy(vertical = vertical - value)
  def moveDown(value: Int) = this.copy(vertical = vertical + value)
  def changeAim(value: Int) = this.copy(aim = aim + value)

  def position = horizontal * vertical

object Submarine:
  def start = Submarine(0, 0, 0)

extension (s: String)
  def input: Seq[(Direction, Int)] =
    val uri = getClass().getClassLoader().getResource(s).toURI()
    val path = Paths.get(uri)
    Files
      .readAllLines(path)
      .asScala
      .map { line =>
        val Array(dir, value) = line.split(' ')
        (Direction.valueOf(dir.capitalize), value.toInt)
      }
      .toSeq

@main
def main =

  def part1 =
    val submarine = "input1".input.foldLeft(Submarine.start) {
      case (submarine, (Direction.Forward, value)) =>
        submarine.moveForward(value)
      case (submarine, (Direction.Up, value)) =>
        submarine.moveUp(value)
      case (submarine, (Direction.Down, value)) =>
        submarine.moveDown(value)
    }
    submarine.position

  def part2 =
    val submarine = "input2".input.foldLeft(Submarine.start) {
      case (submarine, (Direction.Forward, value)) =>
        submarine.moveForward(value).moveDown(value * submarine.aim)
      case (submarine, (Direction.Up, value)) =>
        submarine.changeAim(-value)
      case (submarine, (Direction.Down, value)) =>
        submarine.changeAim(value)
    }
    submarine.position

  println("Part 1")
  println(part1)
  println("Part 2")
  println(part2)

end main
