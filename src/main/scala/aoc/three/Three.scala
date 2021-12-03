package aoc.two

import utils.Constants.BASE_PATH

import java.nio.file.Paths
import scala.Function.tupled
import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs


object Three extends App {

  val filename = Paths.get(BASE_PATH + "/three/input.txt").toAbsolutePath.toString
  val file = Source.fromFile(filename)
  val lines = file.getLines().toList.map(line => line.toList.map(_.asDigit))
  file.close()

  def part1(lst: List[List[Int]]): Int = {

    def sum2Lines(line1: List[Int], line2: List[Int]): List[Int] =
      line1.zip(line2).map(t => t._1 + t._2)

    @tailrec
    def sumAllLines(lst: List[List[Int]]): List[Int] = lst match {
      case head :: Nil => head
      case _ => sumAllLines(sum2Lines(lst.head, lst.tail.head) :: lst.tail.tail)
    }

    def toBin(lst: List[Int]): List[Int] =
      lst.map(e => if (e > 0) 1 else 0)

    def reverseBits(lst: List[Int]): List[Int] =
      lst.map(e => if (e == 0) 1 else 0)


    val sum = sumAllLines(lst.map(line => line.map(n => if (n == 0) -1 else n)))

    val bin = toBin(sum)

    Integer.parseInt(bin.mkString, 2) * Integer.parseInt(reverseBits(bin).mkString, 2)
  }


  def part2(lst: List[List[Int]]): Int = {

    def findMostCommonBitOnPosition(lines: List[List[Int]], index: Int): Int =
      if (lines.map(l => l(index)).map(e => if (e == 0) -1 else 1).sum >= 0) 1 else 0

    def findLeastCommonBitOnPosition(lines: List[List[Int]], index: Int): Int =
      1 - findMostCommonBitOnPosition(lines, index)

    def filterLeast(lines: List[List[Int]], index: Int): List[List[Int]] = {
      val leastCommonBit = findLeastCommonBitOnPosition(lines, index)
      lines.filter(l => l(index) == leastCommonBit)
    }

    def filter(lines: List[List[Int]], index: Int): List[List[Int]] = {
      val mostCommonBit = findMostCommonBitOnPosition(lines, index)
      lines.filter(l => l(index) == mostCommonBit)
    }

    def getOxygenRating(lines: List[List[Int]]): Int = {
      @tailrec
      def filterRec(lines: List[List[Int]], level: Int): List[Int] = {
        if (lines.length == 1)
          lines.head
        else
          filterRec(filter(lines, level), level + 1)
      }

      Integer.parseInt(filterRec(lines, 0).mkString, 2)
    }

    def getCO2Rating(lines: List[List[Int]]): Int = {
      @tailrec
      def filterRec(lines: List[List[Int]], level: Int): List[Int] =
        if (lines.length == 1)
          lines.head
        else
          filterRec(filterLeast(lines, level), level + 1)

      Integer.parseInt(filterRec(lines, 0).mkString, 2)
    }

    getOxygenRating(lines) * getCO2Rating(lines)

  }

  println(part1(lines))
  println(part2(lines))
}

