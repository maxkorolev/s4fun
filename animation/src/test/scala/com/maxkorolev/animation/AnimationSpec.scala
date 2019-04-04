package com.maxkorolev.animation

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext

class AnimationSpec extends FlatSpec with Matchers {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  "animation" should "go" in {

    import Animation._

    animate("..R....", 2) shouldBe Array("..X....",  "....X..",  "......X",  ".......")
    animate("..R.....", 2) shouldBe Array("..X.....",  "....X...",  "......X.",  "........")
    animate("RR..LRL", 3) shouldBe Array("XX..XXX",  ".X.XX..",  "X.....X",  ".......")
    animate("LRLR.LRLR", 2) shouldBe Array("XXXX.XXXX",  "X..X.X..X",  ".X.X.X.X.",  ".X.....X.",  ".........")
    animate("RLRLRLRLRL", 10) shouldBe Array("XXXXXXXXXX",  "..........")
    animate("...", 1) shouldBe Array("...")
    animate("LRRL.LR.LRR.R.LRRL.", 1) shouldBe Array(
      "XXXX.XX.XXX.X.XXXX.",
      "..XXX..X..XX.X..XX.",
      ".X.XX.X.X..XX.XX.XX",
      "X.X.XX...X.XXXXX..X",
      ".X..XXX...X..XX.X..",
      "X..X..XX.X.XX.XX.X.",
      "..X....XX..XX..XX.X",
      ".X.....XXXX..X..XX.",
      "X.....X..XX...X..XX",
      ".....X..X.XX...X..X",
      "....X..X...XX...X..",
      "...X..X.....XX...X.",
      "..X..X.......XX...X",
      ".X..X.........XX...",
      "X..X...........XX..",
      "..X.............XX.",
      ".X...............XX",
      "X.................X",
      "..................."
    )
  }
}
