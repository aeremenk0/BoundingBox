package ru.eremenko

import org.scalatest.{FlatSpec, Matchers}

class MainSpec extends FlatSpec with Matchers {
  import Main._
  val in = """------
             |-****-
             |-***--
             |-****-
             |------""".stripMargin.split("\n").toIterator

  val smallBox = toMatrix(in)
  val emptyInput = toMatrix("".stripMargin.split("\n").toIterator)

  "toMatrix" should "convert list of strings to matrix of Booleans" in {
    val input =
      """**-------***
        |-*--**--***-
        |-----***--**
        |-------***--""".stripMargin.split("\n").toIterator

    val res = toMatrix(input)

    res(0)(0) shouldBe (true)
    res(0)(1) shouldBe (true)
    res(1)(0) shouldBe (false)
    res(1)(1) shouldBe (true)
  }

  "toMatrix" should "return an empty matrix on empty input" in {
    val input = """""".stripMargin.split("\n").toIterator

    val res = toMatrix(input)

    res.size shouldBe 1
    res(0).isEmpty shouldBe (true)
  }
}
