package ru.eremenko

import org.scalatest.{FlatSpec, Matchers}

class BoxSpec extends FlatSpec with Matchers {
  "Box.isOverlaps" should "return true if one boxes overlap" in {
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(2,2), Point(4,4))) shouldBe true
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(4,4), Point(6,6))) shouldBe false
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(4,1), Point(6,3))) shouldBe false
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(1,4), Point(1,3))) shouldBe false
  }
}
