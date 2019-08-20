package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class MaxProductOfThreeSpec extends FlatSpec with Matchers {

  "MaxProductOfThreeSpec" should "check array" in {

    MaxProductOfThree.solution(Array(-3, 1, 2, -2, 5, 6)) shouldBe 60
  }

}
