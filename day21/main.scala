// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6
// using javaOpt -Xss128m

package day21

import scala.util.Using
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

// @main def part2(): Unit =
//   println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day21"))(_.mkString)

def parse(input: String) =
  val Array(first, second) = input.split("\n")
  val firstStart = first.stripPrefix("Player 1 starting position:").trim.toInt
  val secondStart = second.stripPrefix("Player 2 starting position:").trim.toInt
  Game(firstStart, secondStart)

case class Game(firstPosition: Int, secondPosition: Int):
  case class State(position: Int, score: Int)
  val dice = new DeterministicDice()
  def playIt() =
    val first = State(firstPosition, 0)
    val second = State(secondPosition, 0)
    val res = play(first, second)
    res

  @tailrec
  private def play(state1: State, state2: State): Long = 
    val updatedState1 = playPlayer(state1)
    if (updatedState1.score >= 1000)  state2.score.toLong * dice.rolledTimes.toLong
    else 
      val updatedState2 = playPlayer(state2)
      if (updatedState2.score >= 1000)  updatedState1.score.toLong * dice.rolledTimes.toLong
      else play(updatedState1, updatedState2)

  private def playPlayer(state: State) =
    val move = dice.roll() + dice.roll() + dice.roll()
    val newPosition = ((state.position - 1) + move) % 10 + 1
    state.copy(position = newPosition, score = state.score + newPosition)

class DeterministicDice:
  private var value = 1
  private var rolledNum = 0

  def rolledTimes: Int = rolledNum

  def roll(): Int =
    rolledNum += 1
    val rolled = value
    value += 1
    if value == 101 then value = 1
    rolled

def part1(input: String) =
  val game = parse(input)
  game.playIt()

// def part2(input: String) =
//   val cave = parse(input).repeatTile
//   cave.dynamicPaths
// end part2
