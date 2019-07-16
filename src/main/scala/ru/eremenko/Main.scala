package ru.eremenko

import scala.io.Source

object Main {

  def main(args: Array[String]) {
    val xs = for (ln <- Source.stdin.getLines) yield(ln)
    val m = toMatrix(xs)
    val r = BoxDiscovery.solve(m)
    r foreach println
  }

  // transforms input into Vector of Vectors first index is vertical second is horizontal
  def toMatrix(ls: Iterator[String]) : Vector[Vector[Boolean]] = {
    ls.map(_.toCharArray.toList.map( _ == '*').toVector).toVector
  }
}
