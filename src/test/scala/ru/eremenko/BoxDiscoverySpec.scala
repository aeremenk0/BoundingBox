package ru.eremenko

import org.scalatest.{FlatSpec, Matchers}

class BoxDiscoverySpec extends FlatSpec with Matchers {
  import BoxDiscovery._

  "adjacentSet" should "return an empty set if input set is empty" in {
    val r = adjacentSet(Point(0,0), Set.empty)

    r shouldBe 'empty
  }

  "adjacentSet" should "return an empty set if input set if there no adjacent points" in {
    val r = adjacentSet(Point(2,2), Set(Point(1,1), Point(1,3), Point(3,1), Point(3,3)))
    r shouldBe 'empty
  }

  "adjacentSet" should "return a set of adjacent points" in {
    val orig = Set(Point(1,2), Point(2,1), Point(2,3), Point(3,2))
    val r = adjacentSet(Point(2,2), orig)
    r should not be 'Empty
    r.size shouldBe 4

    r shouldBe orig
  }

  "getSet" should "return an empty set on empty input" in {
    val in = """""".stripMargin.split("\n").toIterator

    val r = getSet(Main.toMatrix(in))
    r shouldBe 'empty
  }

  "getSet" should "return an empty set if there are no asterisks in the input" in {
    val in = """------
               |------
               |------
               |------
               |------""".stripMargin.split("\n").toIterator

    val r = getSet(Main.toMatrix(in))
    r shouldBe 'empty
  }

  "getSet" should "return a set of points contains true values" in {
    val in = """------
               |-*----
               |-**---
               |-***--
               |------""".stripMargin.split("\n").toIterator

    val r = getSet(Main.toMatrix(in))
    r should not be 'empty

    r.size shouldBe 6
    r shouldBe Set(Point(1,1),Point(2,1),Point(2,2), Point(3,1), Point(3,2), Point(3,3))
  }

  "discoverContiguousGroup" should "return an empty set if input set is empty" in {
    val r = discoverContiguousGroup(Point(0,0), Set.empty)
    r shouldBe 'empty
  }

  "discoverContiguousGroup" should "return an empty set if there no adjacent points" in {
    val r = discoverContiguousGroup(Point(0,0), Set(Point(1,1), Point(1,3), Point(3,1), Point(3,3)))
    r shouldBe 'empty
  }

  "discoverContiguousGroup" should "return a set of contiguous group" in {
    val in = """*-*---
               |-*-*--
               |-**---
               |-***--
               |*---*-""".stripMargin.split("\n").toIterator

    val set = getSet(Main.toMatrix(in))
    val r = discoverContiguousGroup(Point(1,1), set)
    r should not be 'empty
    r shouldBe Set(Point(1,1), Point(2,1),Point(2,2), Point(3,1), Point(3,2), Point(3,3))
  }

  "discoverAllContiguousGroups" should "return an empty set of contiguous group" in {
    val in = """*-*---
               |-*-*--
               |-**---
               |-***--
               |*---*-""".stripMargin.split("\n").toIterator

    val set = getSet(Main.toMatrix(in))
    val r = discoverAllContiguousGroups(set)
    r should not be 'empty
    r.size shouldBe 6
  }

  "getContiguousGroupBox" should "return a one point box if ContiguousGroups Set is empty" in {
    val orig = Point(0,0)
    val r = getContiguousGroupBox(orig, Set.empty)
    r shouldBe Box(orig, orig)
  }

  "getContiguousGroupBox" should "return a box to minimally cover the group" in {
    val in = """------
               |-*----
               |-**---
               |-***--
               |------""".stripMargin.split("\n").toIterator

    val cg = discoverContiguousGroup(Point(1,1), getSet(toMatrix(in)))

    cg should not be 'empty
    cg shouldBe Set(Point(1,1), Point(2,1),Point(2,2), Point(3,1), Point(3,2), Point(3,3))

    val r = getContiguousGroupBox(cg.head, cg.tail)

    r shouldBe Box(Point(1,1), Point(3,3))
  }

  "solve" should "return List of Boxes" in {
    {
      val input =
        """**-------***
          |-*--**--***-
          |-----***--**
          |-------***--""".stripMargin.split("\n").toIterator

      val m = Main.toMatrix(input)
      val r = solve(m)

      r should not be 'empty
      r.size shouldBe(1)

      r shouldBe List(Box(Point(0,0), Point(1,1)))
    }
    {
      val input =
        """-*-
          |***
          |-*-
        """.stripMargin.split("\n").toIterator

      val m = Main.toMatrix(input)
      val r = solve(m)

      r should not be 'empty
      r.size shouldBe(1)

      r shouldBe List(Box(Point(0,0), Point(2,2)))
    }
    {
      val input =
        """---**
          |***-*
          |-***-
        """.stripMargin.split("\n").toIterator

      val m = Main.toMatrix(input)
      val r = solve(m)

      r shouldBe'empty
    }

    {
      val input =
        """*-*--
          |-*-*-
          |-**-*
          |-***-
        """.stripMargin.split("\n").toIterator

      val m = Main.toMatrix(input)
      val r = solve(m)

      r should not be 'empty
      r.size shouldBe(1)

      r shouldBe List(Box(Point(1,1), Point(3,3)))
    }
  }
}
