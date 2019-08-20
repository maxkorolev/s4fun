package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class MinMaxDivisionSpec extends FlatSpec with Matchers {

  "MinMaxDivisionSpec" should "check array" in {

    // MinMaxDivision.solution(3, 10, Array(1, 1, 1)) shouldBe 1
    // MinMaxDivision.solution(3, 10, Array(9, 8, 1, 2, 3)) shouldBe 6
  }

}
