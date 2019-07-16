package ru.eremenko

import scala.io.Source

object Main {

  def main(args: Array[String]) {
    val xs = for (ln <- Source.stdin.getLines) yield(ln)
    val m = toMatrix(xs)
    val r = solve(m)
    r foreach println
  }

  def solve(m: Vector[Vector[Boolean]]): List[Box] = {
    import BoxDiscovery._
    val s = getSet(m)
    val cgs = discoverAllContiguousGroups(s)
    val bb = cgs.filterNot(_.size < 2).map(s => getContiguousGroupBox(s.head, s.tail))
    removeOverlappingBoxes(bb)
  }

  // transforms input into Vector of Vectors first index is vertical second is horizontal
  def toMatrix(ls: Iterator[String]) : Vector[Vector[Boolean]] = {
    ls.map(_.toCharArray.toList.map( _ == '*').toVector).toVector
  }

  def removeOverlappingBoxes(ls: List[Box]): List[Box] = {
    def go(xs: List[Box], res: List[Box]): List[Box] =
      xs match {
        case Nil => res
        case h::t =>
          val filtered = t.filterNot(h.isOverlaps(_))
          if (filtered.size == t.size) {
            go(filtered, h::res)
          } else {
            go(filtered, res)
          }
      }

    go(ls, Nil)
  }
}
