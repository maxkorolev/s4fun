package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class TapeEquilibriumSpec extends FlatSpec with Matchers {

  "TapeEquilibriumSpec" should "check array" in {

    TapeEquilibrium.solution(Array(3, 1, 2, 4, 3)) shouldBe 1
    TapeEquilibrium.solution(Array(3, 1)) shouldBe 2
  }

}
