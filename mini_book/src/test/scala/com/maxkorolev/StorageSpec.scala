package com.maxkorolev

import org.scalatest._
import cats.effect._

import scala.concurrent.ExecutionContext

class StorageSpec extends FlatSpec with Matchers {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  "Storage" should "be able add, update and delete elements" in {

    (for {
      storage <- Storage[IO, String, Int]

      _ <- storage.add("Q1", 123)
      _ <- storage.add("Q2", 231)
      _ <- storage.add("Q3", 456)
      _ <- storage.update("Q2", 777)
      _ <- storage.delete("Q3")
      lst <- storage.getAll

    } yield {
      lst shouldEqual List(123, 777)
    }).unsafeRunSync()
  }
}
