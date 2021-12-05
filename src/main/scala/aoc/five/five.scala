package aoc.five

import aoc.five.HydrothermalVent.createHydrothermalVent
import utils.Constants.BASE_PATH

import java.nio.file.Paths
import scala.io.Source;


case class HydrothermalVent(begin: (Int, Int), end: (Int, Int)) {
  def isVertical: Boolean =
    (begin._1 == end._1)

  def isHorizontal: Boolean =
    (begin._2 == end._2)

  def isDiagonal: Boolean =
    Math.abs(begin._1 - begin._2) == Math.abs(end._1 - end._2)

  def getPoints: List[(Int, Int)] = {
    if (this.isVertical) {
      val min = Math.min(begin._2, end._2)
      val max = Math.max(begin._2, end._2)
      return (min to max).map(v => (begin._1, v)).toList
    }
    if (this.isHorizontal) {
      val min = Math.min(begin._1, end._1)
      val max = Math.max(begin._1, end._1)
      return (min to max).map(v => (v, begin._2)).toList
    }
    val xDir = if (begin._1 < end._1) 1 else -1
    val yDir = if (begin._2 < end._2) 1 else -1
    val size = Math.abs(begin._1 - end._1)
    (0 to size).map(s => (begin._1 + s * xDir, begin._2 + s * yDir)).toList
  }
}

object HydrothermalVent {
  def createHydrothermalVent(line: String): HydrothermalVent = {
    val beginEnd = line.split(" -> ")
    val begin = beginEnd(0).split(",").map(_.toInt)
    val end = beginEnd(1).split(",").map(_.toInt)
    HydrothermalVent((begin(0), begin(1)), (end(0), end(1)))
  }
}

object Five extends App {

  val filename = Paths.get(BASE_PATH + "/five/input.txt").toAbsolutePath.toString
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  file.close()

  val vents = lines.map(createHydrothermalVent)

  def part1: Int = {
    val pointGroups = vents.filter(v => v.isVertical || v.isHorizontal)
      .flatMap(_.getPoints)
      .groupBy(identity)

    pointGroups.values.count(v => v.length > 1)
  }

  def part2: Int = {
    val pointGroups = vents
      .flatMap(_.getPoints)
      .groupBy(identity)

    pointGroups.values.count(v => v.length > 1)
  }

  println(part1)
  println(part2)
}