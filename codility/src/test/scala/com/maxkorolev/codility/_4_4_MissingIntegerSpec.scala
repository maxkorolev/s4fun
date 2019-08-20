package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class MissingIntegerSpec extends FlatSpec with Matchers {

  "MissingIntegerSpec" should "check array" in {

    MissingInteger.solution(Array(1, 2, 3)) shouldBe 4
    MissingInteger.solution(Array(1, 3, 6, 4, 1, 2)) shouldBe 5
    MissingInteger.solution(Array(-1, -3)) shouldBe 1
    MissingInteger.solution(Array.emptyIntArray) shouldBe 1
  }

}
