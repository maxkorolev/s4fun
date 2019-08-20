package com.maxkorolev.codility

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import java.time.Instant

class BracketsSpec extends FlatSpec with Matchers {

  "BracketsSpec" should "check array" in {

    Brackets.solution("(())()(())") shouldBe 1
    Brackets.solution("()()()") shouldBe 1
    Brackets.solution("((()()") shouldBe 0
    Brackets.solution("{[()()]}") shouldBe 1
    Brackets.solution("([)()]") shouldBe 0
    Brackets.solution("") shouldBe 1
  }

}
