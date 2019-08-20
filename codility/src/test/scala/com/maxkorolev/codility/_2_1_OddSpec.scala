package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class OddSpec extends FlatSpec with Matchers {

  "OddSpec" should "check array" in {

    Odd.solution(Array(1, 2, 1, 2, 3)) shouldBe 3
  }

}
