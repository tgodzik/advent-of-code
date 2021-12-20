// using scala 3.1.0
// using lib com.lihaoyi::pprint:0.6.6
// using lib org.typelevel::cats-parse:0.3.6"
// using javaOpt -Xss128m

package day18

import cats.parse.Numbers
import cats.parse.Parser0
import cats.parse.Rfc5234.alpha
import cats.parse.Rfc5234.digit
import cats.parse.Rfc5234.sp
import cats.parse.{Parser as P}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

@main def part1(): Unit =
  println(s"The solution is ${part1(readInput())}")

@main def part2(): Unit =
  println(s"The solution is ${part2(readInput())}")

def readInput(): String =
  Using.resource(Source.fromFile("input/day18"))(_.mkString)

object NumberParser:

  val parser = P.recursive[Expr] { recurse =>
    val num: P[Value] = digit.rep.map(l => Value(l.toList.mkString.toInt))

    // def pair(pa: P[Expr.Pair]): P[Expr.Pair] = (openBracket *> (pair | num) ~ coma ~ (pair | num) <* closeBracket)
    def rep[A](pa: P[A]): P[((A, Unit), A)] =
      pa ~ P.char(',') ~ pa

    val pair = rep(recurse).with1
      .between(P.char('['), P.char(']'))
      .map { case ((first, _), second) =>
        Pair(first, second)

      }
    P.oneOf(num :: pair :: Nil)

  }

  def parse(input: String): List[Expr] =
    val allExprs = for line <- input.split("\n") yield NumberParser.parser.parse(line) match
      case Right(expr) => expr(1)
      case _           => throw new IllegalArgumentException(line)
    allExprs.toList
  end parse
end NumberParser

trait Expr:
  override def toString() =
    this match
      case Value(num)        => num.toString
      case Pair(left, right) => s"[$left,$right]"

  def view: Array[Value] =
    this match
      case v: Value          => Array(v)
      case Pair(left, right) => left.view ++ right.view

  def copy(): Expr =
    this match
      case Pair(expr1, expr2) =>
        Pair(expr1.copy(), expr2.copy())
      case Value(num) =>
        Value(num)

  def add(other: Expr) = Pair(this, other)

class Value(var num: Int) extends Expr:
  def add(i: Int) = num += i

object Value:
  def unapply(e: Expr): Option[Int] =
    e match
      case v: Value => Some(v.num)
      case _        => None

class Pair(val left: Expr, val right: Expr) extends Expr

object Pair:
  def unapply(e: Expr): Option[(Expr, Expr)] =
    e match
      case v: Pair => Some((v.left, v.right))
      case _       => None

enum Operation:
  case Explode(pair: Pair, replacement: Value)
  case Split(value: Value, replacement: Pair)

class SnaifishNumber(topLevel: Expr):
  private var view = Array(topLevel.view*)
  private var currentTopLevel = topLevel
  def add(other: Expr) =
    currentTopLevel = currentTopLevel.add(other)
    checkOperation()


  private def replace(exprToRemove: Expr, replacement: Expr) =
    def loop(expr: Expr): Expr =
      expr match
        case expr if expr == exprToRemove =>
          replacement
        case Pair(expr1, expr2) =>
          Pair(loop(expr1), loop(expr2))
        case other =>
          other
    currentTopLevel = loop(currentTopLevel)

  private def checkExplode(expr: Expr, level: Int): Option[Operation] =
    expr match
      case pair @ Pair(v1 @ Value(n1), v2 @ Value(n2)): Pair if level > 3 =>
        Some(Operation.Explode(pair, Value(0)))
      case Pair(p1, p2) =>
        checkExplode(p1, level + 1).orElse(checkExplode(p2, level + 1))
      case v: Value =>
        None

  private def checkSplit(expr: Expr): Option[Operation] =
    expr match
      case v: Value if v.num > 9 =>
        val left = Math.floor(v.num.toDouble / 2.0).toInt
        val right = Math.ceil(v.num.toDouble / 2.0).toInt
        Some(Operation.Split(v, Pair(Value(left), Value(right))))
      case Pair(p1, p2) =>
        checkSplit(p1).orElse(checkSplit(p2))
      case _ =>
        None

  def magnitiude(expr: Expr = currentTopLevel): Int =
    expr match
      case Value(num)   => num
      case Pair(p1, p2) => 3 * magnitiude(p1) + 2 * magnitiude(p2)

  @tailrec
  private def checkOperation(): Unit =
    view = currentTopLevel.view
    checkExplode(currentTopLevel, 0).orElse(checkSplit(currentTopLevel)) match
      case Some(Operation.Explode(p @ Pair(v1: Value, v2: Value), replacement: Value)) =>
        val indexV1 = view.indexOf(v1)
        if indexV1 > 0 then view(indexV1 - 1).add(v1.num)
        val indexV2 = view.indexOf(v2)
        if indexV2 < view.size - 1 then view(indexV2 + 1).add(v2.num)
        replace(p, replacement)
        checkOperation()

      case Some(Operation.Split(v, p @ Pair(v1: Value, v2: Value))) =>
        val indexOfV = view.indexOf(v)
        replace(v, p)
        checkOperation()
      case _ =>
    end match
  end checkOperation

  override def toString = currentTopLevel.toString
end SnaifishNumber

def part1(input: String) =
  val allexprs = NumberParser.parse(input)
  val snail = SnaifishNumber(allexprs.head)
  allexprs.tail.foldLeft(snail) { case (snail, expr) =>
    snail.add(expr)
    snail
  }
  snail.magnitiude()
end part1

def part2(input: String) =

  val allexprs = NumberParser.parse(input)

  val all = for
    num1 <- allexprs
    num2 <- allexprs
    if num1 != num2
  yield
    val n1 = num1.copy()
    val n2 = num2.copy()
    val snail = SnaifishNumber(n1)
    snail.add(n2)
    snail.magnitiude()
  all.max
end part2
