// using resource "./resources"
// using lib com.lihaoyi::pprint:0.6.6
// using lib org.scala-lang.modules::scala-parallel-collections:1.0.4

package day6

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import java.util.concurrent.atomic.AtomicInteger

extension (s: String)
  def input: List[Int] =
    val uri = this.getClass().getClassLoader().getResource(s).toURI()
    val path = Paths.get(uri)
    Files.readAllLines(path).asScala.head.split(",").toList.map(_.toInt)

def howManyFish(days: Int) =
  val fish = "input1".input
  val size = fish.size
  val currentCalculated = new AtomicInteger(0)
  fish.zipWithIndex.par.map { case (f, i) =>
    def fishIteration(daysLeft: Int): Long =
      if daysLeft > 0 then
        1 + fishIteration(daysLeft - 9) + fishIteration(daysLeft - 7)
      else 0

    val desendants = 1 + fishIteration(days - f)
    println(s"${100 * currentCalculated.incrementAndGet / size.toDouble}%")
    desendants
  }.sum
end howManyFish

@main
def main =
  val part1 = howManyFish(80)
  pprint.log(part1)
  val part2 = howManyFish(256)
  pprint.log(part2)

end main
