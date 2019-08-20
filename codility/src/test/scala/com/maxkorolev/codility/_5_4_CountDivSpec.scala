package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class CountDivSpec extends FlatSpec with Matchers {

  "CountDivSpec" should "check array" in {

    CountDiv.solution(6, 11, 2) shouldBe 3
    CountDiv.solution(6, 11, 15) shouldBe 0
    CountDiv.solution(5, 10, 7) shouldBe 1
  }

}
