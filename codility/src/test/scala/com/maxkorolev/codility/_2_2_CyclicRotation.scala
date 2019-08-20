package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class CyclicRotationSpec extends FlatSpec with Matchers {

  "CyclicRotationSpec" should "check array" in {

    CyclicRotation.solution(Array(), 0) shouldBe Array()
    CyclicRotation.solution(Array(), 1) shouldBe Array()
    CyclicRotation.solution(Array(1, 2), 1) shouldBe Array(2, 1)
    CyclicRotation.solution(Array(1, 2, 3, 4), 2) shouldBe Array(3, 4, 1, 2)
    CyclicRotation.solution(Array(1, 2, 3), 3) shouldBe Array(1, 2, 3)
    CyclicRotation.solution(Array(0, 0, 0), 1) shouldBe Array(0, 0, 0)

    val bigArray = (0 to 4999).toArray
    CyclicRotation.solution(bigArray, 5000) shouldBe bigArray
  }

}
