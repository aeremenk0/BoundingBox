package ru.eremenko

object AdjacentDiscovery {
  def adjacentSet(p: Point, ps: Set[Point]) : Set[Point] = {
    val t = Set(Point(p.v-1, p.h), Point(p.v, p.h-1), Point(p.v+1, p.h), Point(p.v, p.h+1))
    t.filter(ps)
  }

  def discoverContiguousGroup(p: Point, ps: Set[Point], acc: Set[Point]=Set.empty) : Set[Point] = {
    if (ps.isEmpty) {
      acc
    } else {
      val r = adjacentSet(p, ps)

      if (r.isEmpty) {
        acc
      } else {
        r.flatMap(x => discoverContiguousGroup(x, ps -- r, acc ++ r))
      }
    }
  }

  def discoverAllContiguousGroups(ps: Set[Point], acc: List[Set[Point]] = List.empty ): List[Set[Point]] = {
    ps.headOption match {
      case None => acc
      case Some(p) =>
        val cg = discoverContiguousGroup(p, ps - p, Set(p))
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

  def getSet(m: Vector[Vector[Boolean]]): Set[Point] = {
    val xs = for {
      v <- 0 to (m.size - 1)
      h <- 0 to (m(v).size - 1) if m(v)(h)
    } yield Point(v, h)
    xs.toSet
  }
}
