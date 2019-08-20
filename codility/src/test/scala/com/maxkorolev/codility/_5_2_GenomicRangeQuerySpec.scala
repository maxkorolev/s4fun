package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class GenomicRangeQuerySpec extends FlatSpec with Matchers {

  "GenomicRangeQuerySpec" should "check array" in {

    GenomicRangeQuery.solution("CAGCCTA", Array(2, 5, 0), Array(4, 5, 6)) shouldBe Array(
      2,
      4,
      1
    )
  }

}
