// using resource "./resources"

package day4

import java.nio.file.Files
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class Winner(winnerNum: Int, score: Int)

case class Board(
    horizontal: Seq[mutable.Set[Int]],
    vertical: Seq[mutable.Set[Int]]
):
  var isWon: Boolean = false
  def toWinner(num: Int) =
    Winner(horizontal.map(_.sum).sum, num)

object Board:
  def apply(horizontal: Seq[Seq[Int]]): Board =
    val horizontalSets = horizontal.map(seq => mutable.Set(seq*))
    def empty = mutable.Set.empty[Int]
    val vertical = Seq(empty, empty, empty, empty, empty)
    horizontal.foreach { list =>
      list.zipWithIndex.foreach { case (num, index) =>
        vertical(index) += num
      }
    }

    Board(horizontalSets, vertical)

case class Bingo(order: Seq[Int], boards: Seq[Board]):
  var won = 0

  @tailrec
  final def play(
      current: Seq[Int] = order,
      winLastRule: Boolean
  ): Option[Winner] =
    current match
      case num :: tail =>
        val winners = boards.map { board =>
          def removeNum(line: mutable.Set[Int]): Boolean =
            line -= num
            if line.isEmpty then true
            else false

          val winHorizontal =
            board.horizontal.collect(removeNum _).exists(identity)
          val winVertical =
            board.vertical.collect(removeNum _).exists(identity)

          if !board.isWon && (winVertical || winHorizontal) then
            board.isWon = true
            won += 1
            Some(board)
          else None
        }.flatten
        winners.headOption match
          case Some(board) =>
            if winLastRule && tail.nonEmpty && won < boards.size then
              val lastWon = Some(board.toWinner(num))
              play(tail, winLastRule)
            else Some(board.toWinner(num))
          case None =>
            play(tail, winLastRule)
      case _ => None
end Bingo

extension (s: String)
  def input: Bingo =
    val uri = getClass().getClassLoader().getResource(s).toURI()
    val path = Paths.get(uri)
    val lines = Files.readAllLines(path).asScala.toList

    val order = lines.head.split(",").toList.map(_.toInt)
    val boards = lines.tail.grouped(6).map { case boardNums =>
      boardNums.filter(_.nonEmpty) match
        case horizontal if horizontal.size == 5 =>
          Board(horizontal.map(_.trim.split("\\s+").toIndexedSeq.map(_.toInt)))
        case _ => throw new Exception(s"Error parsing, got $boardNums")
    }
    Bingo(order, Seq(boards.toSeq*))

@main
def main =
  def part1 =
    val bingo = "input1".input
    val winner = bingo.play(winLastRule = false)
    winner match
      case Some(win) =>
        println(win)
        println(win.score * win.winnerNum) // 38594
      case _ =>
        println("Thre was no winner!")

  def part2 =
    val bingo = "input1".input
    val looser = bingo.play(winLastRule = true)
    looser match
      case Some(looser) =>
        println(looser)
        println(looser.score * looser.winnerNum)
      case _ =>
        println("Thre was no looser!")
  part1
  part2
end main
