package ru.eremenko

import org.scalatest.{FlatSpec, Matchers}

class BoxSpec extends FlatSpec with Matchers {
  "Box.area" should "return Box area" in {
    Box(Point(1, 1), Point(2, 2)).area shouldBe 1
    Box(Point(1, 1), Point(3, 3)).area shouldBe 4
    Box(Point(1, 1), Point(2, 3)).area shouldBe 2
    Box(Point(1, 1), Point(1, 3)).area shouldBe 0
  }

  "Box.isCorrect" should "return true if box has area > 0" in {
    Box(Point(1, 1), Point(1, 3)).isCorrect shouldBe false
    Box(Point(1, 1), Point(2, 3)).isCorrect shouldBe true
  }

  "Box.isContains" should "return true if one box contains the other" in {
    Box(Point(1,1), Point(3,3)).isContains(Box(Point(1,1), Point(2,2))) shouldBe true
    Box(Point(1,1), Point(3,3)).isContains(Box(Point(1,1), Point(4,4))) shouldBe false
    Box(Point(0,0), Point(3,3)).isContains(Box(Point(1,1), Point(4,4))) shouldBe false

    Box(Point(1,1), Point(3,3)).isContains(Box(Point(1,1), Point(1,1))) shouldBe true
    Box(Point(1,1), Point(4,4)).isContains(Box(Point(2,2), Point(3,3))) shouldBe true
  }

  "Box.isOverlaps" should "return true if one boxes overlap" in {
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(2,2), Point(4,4))) shouldBe true
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(4,4), Point(6,6))) shouldBe false
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(4,1), Point(6,3))) shouldBe false
    Box(Point(1,1), Point(3,3)).isOverlaps(Box(Point(1,4), Point(1,3))) shouldBe false
  }
}
