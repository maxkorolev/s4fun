package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class PassingCarsSpec extends FlatSpec with Matchers {

  "PassingCarsSpec" should "check array" in {

    PassingCars.solution(Array(0, 1, 0, 1, 1)) shouldBe 5
  }

}
