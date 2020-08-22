package ru.eremenko

object BoxDiscovery {
  def adjacentSet(p: Point, ps: Set[Point]) : Set[Point] = {
    val t = Set(Point(p.v-1, p.h), Point(p.v, p.h-1), Point(p.v+1, p.h), Point(p.v, p.h+1), p)
    t.filter(ps)
  }

  def discoverContiguousGroup(p: Point, ps: Set[Point]) : Set[Point] = {
    val r = adjacentSet(p, ps)

    if (r.isEmpty) {
      r
    } else {
      r.flatMap(x => r ++ discoverContiguousGroup(x, ps -- r))
    }
  }

  def discoverAllContiguousGroups(ps: Set[Point], acc: List[Set[Point]] = List.empty ): List[Set[Point]] = {
    ps.headOption match {
      case None => acc
      case Some(p) =>
        val cg = discoverContiguousGroup(p, ps)
        discoverAllContiguousGroups(ps -- cg, cg::acc)
    }
  }

  def getContiguousGroupBox(p: Point, cg: Set[Point]): Box = {
    cg.foldLeft(Box(p, p)){ case(b, e) =>
      val topRight = Point(
        if(e.v < b.topLeft.v) e.v else b.topLeft.v,
        if(e.h < b.topLeft.h) e.h else b.topLeft.h
      )
      val bottomLeft = Point(
        if(e.v > b.bottomRight.v) e.v else b.bottomRight.v,
        if(e.h > b.bottomRight.h) e.h else b.bottomRight.h
      )
      Box(topRight, bottomLeft)
    }
  }

  def getPointsSet(m: Vector[Vector[Boolean]]): Set[Point] = {
    val xs = for {
      v <- 0 to (m.size - 1)
      h <- 0 to (m(v).size - 1) if m(v)(h)
    } yield Point(v, h)
    xs.toSet
  }

  def solve(m: Vector[Vector[Boolean]]): List[Box] = {
    val s = getPointsSet(m)
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
