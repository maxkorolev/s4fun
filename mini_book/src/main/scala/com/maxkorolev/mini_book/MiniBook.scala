package com.maxkorolev.mini_book

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.functor._

object MiniBook extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {



    IO.delay(println("hello")) as ExitCode.Success
  }
}
