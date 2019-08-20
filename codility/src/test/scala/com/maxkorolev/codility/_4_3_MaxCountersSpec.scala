package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class MaxCountersSpec extends FlatSpec with Matchers {

  "MaxCountersSpec" should "check array" in {

    MaxCounters.solution(5, Array(3, 4, 4, 6, 1, 4, 4)) shouldBe Array(
      3,
      2,
      2,
      4,
      2
    )
  }

}
