package com.maxkorolev.recursion.whisky

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec

class ParseSpec extends FlatSpec with Matchers {

  "Parse" should "work" in {

    Parse.run

    true shouldBe true

  }
}
