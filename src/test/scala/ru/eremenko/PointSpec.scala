package ru.eremenko

import org.scalatest.{FlatSpec, Matchers}


class PointSpec extends FlatSpec with Matchers {
  "Point.hLen" should "return 0 for for the same point" in {
    val p = Point(1, 1)

    val r = p.hLen(p)

    r shouldBe 0
  }

  "Point.hLen" should "return horizontal distance between two points" in {
    val p = Point(1, 1)
    val l = Point(3, 3)
    val r = p.hLen(l)

    r shouldBe 2
  }
}