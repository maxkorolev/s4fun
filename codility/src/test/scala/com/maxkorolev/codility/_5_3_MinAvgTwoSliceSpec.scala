package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class MinAvgTwoSliceSpec extends FlatSpec with Matchers {

  "MinAvgTwoSliceSpec" should "check array" in {

    MinAvgTwoSlice.solution(Array(4, 2, 2, 5, 1, 5, 8)) shouldBe 1
  }

}
