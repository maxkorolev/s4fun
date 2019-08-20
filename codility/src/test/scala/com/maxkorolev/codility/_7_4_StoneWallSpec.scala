package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class StoneWallSpec extends FlatSpec with Matchers {

  "StoneWallSpec" should "check array" in {

    StoneWall.solution(Array(10)) shouldBe 1
    StoneWall.solution(Array(3, 2, 1)) shouldBe 3
    StoneWall.solution(Array(1, 0, 1)) shouldBe 2
    StoneWall.solution(Array(0, 0, 1)) shouldBe 1
    StoneWall.solution(Array(1, 2, 1, 2, 1, 2, 1, 2)) shouldBe 5
    StoneWall.solution(Array(1, 1, 1, 2, 2, 2, 1, 1, 1)) shouldBe 2
    StoneWall.solution(Array(1, 2, 0, 0, 0)) shouldBe 2
    StoneWall.solution(Array(0, 0, 0)) shouldBe 0
    StoneWall.solution(Array(1, 2, 3, 2, 0, 0, 0, 5)) shouldBe 4
    StoneWall.solution(Array(1, 2, 3)) shouldBe 3
    StoneWall.solution(Array(3, 4, 3)) shouldBe 2
    StoneWall.solution(Array(3, 4, 5, 4, 3)) shouldBe 3
    StoneWall.solution(Array(3, 4, 5, 2, 3)) shouldBe 5
    StoneWall.solution(Array(8, 8, 5, 7, 9, 8, 7, 4, 8)) shouldBe 7
  }

  "StoneWallSpec2" should "check array" in {

    StoneWall.budgetShopping(50, Array(20, 19), Array(24, 20)) shouldBe 40
    StoneWall.budgetShopping(50, Array(20, 19), Array(34, 20)) shouldBe 38
    StoneWall.budgetShopping(50, Array(20), Array(34)) shouldBe 20
    StoneWall.budgetShopping(50, Array(100), Array(100)) shouldBe 0
    StoneWall.budgetShopping(0, Array(100), Array(100)) shouldBe 0
  }

}
