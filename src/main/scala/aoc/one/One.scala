package aoc.one

import utils.Constants.BASE_PATH

import java.nio.file.Paths
import scala.io.Source




object One extends App {

  val filename = Paths.get(BASE_PATH + "/one/input.txt").toAbsolutePath.toString
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList.map(_.toInt)
  file.close()

  def countIncrease(lst: List[Int]) : Int =
    lst.tail.foldLeft((0, lst.head))((acc, next) => acc match {
      case x if (next > acc._2) => (acc._1 + 1, next)
      case _ => (acc._1, next)
    })._1

  println(countIncrease(lines))

  val sum3 = (lines.sliding(3).map(_.sum).toList)

  println(countIncrease(sum3))

}

