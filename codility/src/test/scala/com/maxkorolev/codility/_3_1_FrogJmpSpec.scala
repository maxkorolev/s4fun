package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class FrogJmpSpec extends FlatSpec with Matchers {

  "FrogJmpSpec" should "check array" in {

    FrogJmp.solution(10, 85, 30) shouldBe 3
    FrogJmp.solution(10, 10, 30) shouldBe 0
    FrogJmp.solution(10, 11, 30) shouldBe 1
  }

}
