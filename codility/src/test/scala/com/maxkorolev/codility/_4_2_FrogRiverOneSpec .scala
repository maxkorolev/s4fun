package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class FrogRiverOneSpec extends FlatSpec with Matchers {

  "FrogRiverOneSpec" should "check array" in {

    FrogRiverOne.solution(5, Array(1, 3, 1, 4, 2, 3, 5, 4)) shouldBe 6

    val curr = Instant.now.toEpochMilli
    val res = FrogRiverOne.solution(500, 500 +: (1 to 499).toArray)
    val time = Instant.now.toEpochMilli - curr
    time should be < 5000L
    res shouldBe 499
  }

}
