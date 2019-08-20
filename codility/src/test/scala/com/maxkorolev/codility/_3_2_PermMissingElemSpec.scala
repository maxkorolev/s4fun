package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class PermMissingElemSpec extends FlatSpec with Matchers {

  "PermMissingElemSpec" should "check array" in {

    PermMissingElem.solution(Array(1, 2, 3)) shouldBe 4
    PermMissingElem.solution(Array(1, 2, 5, 3)) shouldBe 4
  }

}
