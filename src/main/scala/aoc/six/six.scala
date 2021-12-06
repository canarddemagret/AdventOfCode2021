package aoc.six

import aoc.six.LanternFish.{BABY_TIMING, TIMING, createLanternFish}
import utils.Constants.BASE_PATH

import java.nio.file.Paths
import scala.annotation.tailrec
import scala.io.Source;


case class LanternFish(timing: Int) {
  def passDay: List[LanternFish] = {
    if (this.timing == 0) List(LanternFish(BABY_TIMING), LanternFish(TIMING))
    else List(LanternFish(this.timing - 1))
  }
}

object LanternFish {
  val BABY_TIMING = 8
  val TIMING = 6

  def createLanternFish(value: String): LanternFish =
    LanternFish(value.toInt)
}

object Six extends App {

  val filename = Paths.get(BASE_PATH + "/six/input.txt").toAbsolutePath.toString
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  file.close()

  val fishes = lines.head.split(",").map(createLanternFish).toList

  @tailrec
  def part1(fishes: List[LanternFish], days: Int): Int = {
    if (days == 0)
      fishes.length
    else
      part1(fishes.flatMap(_.passDay), days - 1)
  }

  println(part1(fishes, 80))

  def part2(fishes: List[LanternFish], days: Int): Long = {

    @tailrec
    def rec(fishGroups: List[(Int, Long)], days: Int): Long = {
      if (days == 0)
        fishGroups.map(_._2).sum
      else
        rec(
          fishGroups.flatMap(g => g match {
            case (0, x) => List((TIMING, x), (BABY_TIMING, x))
            case (x, y) => List((x-1, y))
          }).groupMapReduce(_._1)(_._2)(_+_).toList
        ,days-1)
    }

    val map = fishes.map(_.timing).groupMapReduce(identity)(_ => 1L)(_+_).toList
    rec(map, days)
  }

  println(part2(fishes, 256))
}