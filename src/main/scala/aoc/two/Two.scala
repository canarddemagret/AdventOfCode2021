package aoc.two

import utils.Constants.BASE_PATH

import java.nio.file.Paths
import scala.io.Source
import scala.math.abs


object Two extends App {

  val filename = Paths.get(BASE_PATH + "/two/input.txt").toAbsolutePath.toString
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList.map(line => {
    val split = line.split(" ")
    (split(0), split(1).toInt)
  })
  file.close()

  def part1(lst: List[(String, Int)]) : Int = {
    val pos = lst.foldLeft((0,0))((acc, next) => next._1 match {
      case "up" => (acc._1 - next._2, acc._2)
      case "down" => (acc._1 + next._2, acc._2)
      case "forward" => (acc._1, acc._2 + next._2)
    })
    pos._1 * pos._2
  }


  case class Position(aim: Int, horizontal: Int, depth: Int)

  def part2(lst: List[(String, Int)]) : Int = {
    val pos = lst.foldLeft(Position(0, 0,0))((pos, cmd) => cmd._1 match {
      case "up" => Position(pos.aim-cmd._2, pos.horizontal, pos.depth)
      case "down" => Position(pos.aim+cmd._2, pos.horizontal, pos.depth)
      case "forward" => Position(pos.aim, pos.horizontal+cmd._2, pos.depth+pos.aim*cmd._2)
    })
    pos.horizontal * pos.depth
  }

  println(part1(lines))
  println(part2(lines))
}

