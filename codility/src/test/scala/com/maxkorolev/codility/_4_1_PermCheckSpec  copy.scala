package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class PermCheckSpec extends FlatSpec with Matchers {

  "PermCheckSpec" should "check array" in {

    PermCheck.solution(Array(3, 1, 2, 4)) shouldBe 1
    PermCheck.solution(Array(3, 1)) shouldBe 0
  }

}
