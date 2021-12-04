package aoc.four

import utils.Constants.BASE_PATH

import java.nio.file.Paths
import scala.annotation.tailrec
import scala.io.Source;


case class Matrix(elements: Set[Int], lines: List[List[Int]], columns: List[List[Int]]) {
  def this(lines: List[List[Int]]) =
    this(
      lines.flatMap(_.map(o => o)).toSet,
      lines,
      lines.transpose
    )

  def addLine(line: List[Int]): Matrix =
    new Matrix(line :: this.lines)

  def removeElement(el: Int): Matrix =
    Matrix(
      this.elements.filter(e => e != el),
      lines.map(lst => lst.filter(e => e != el)),
      columns.map(lst => lst.filter(e => e != el))
    )

  def hasWin: Boolean =
    this.lines.exists(lst => lst.isEmpty) || this.columns.exists(lst => lst.isEmpty)

  def getScore: Int =
    this.elements.sum
}

object Four extends App {

  val filename = Paths.get(BASE_PATH + "/four/input.txt").toAbsolutePath.toString
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList
  file.close()

  val rounds = lines.head.split(",").map(_.toInt).toList

  val allMatrix = lines.tail.foldLeft(List[Matrix]())((matrix, line) => line match {
    case "" => new Matrix(List()) :: matrix
    case _ => matrix.head.addLine(line.split(" ").filter(_.nonEmpty).map(_.toInt).toList) :: matrix.tail
  })

  def getWinner(matrices: List[Matrix]): Option[Matrix] =
    matrices.find(_.hasWin)

  @tailrec
  def runGame1(matrices: List[Matrix], round: Int, restRounds: List[Int]): Int = {
    val afterRound = matrices.map(_.removeElement(round))
    val winner = getWinner(afterRound)
    if (winner.isDefined)
      winner.get.getScore * round
    else
      runGame1(afterRound, restRounds.head, restRounds.tail)
  }

  println(runGame1(allMatrix, rounds.head, rounds.tail))

  @tailrec
  def runGame2(matrices: List[Matrix], lastWinner: (Matrix, Int), round: Int, restRounds: List[Int]): Int = {
    if (restRounds.isEmpty)
      return lastWinner._1.getScore * lastWinner._2
    val afterRound = matrices.map(_.removeElement(round))
    val winner = getWinner(afterRound)
    if (winner.isDefined)
      runGame2(afterRound.filter(e => !e.hasWin), (winner.get, round), restRounds.head, restRounds.tail)
    else
      runGame2(afterRound.filter(e => !e.hasWin), lastWinner, restRounds.head, restRounds.tail)
  }

  println(runGame2(allMatrix, (new Matrix(List()), 0), rounds.head, rounds.tail))
}
