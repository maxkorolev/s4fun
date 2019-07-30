package com.maxkorolev.mini_book

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext

case class Key(value: String) extends AnyVal
case class Value(value: Int) extends AnyVal

class StorageQuerySpec extends FlatSpec with Matchers {

  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  "StorageQuery" should "be able get sorted list" in {

    implicit val orderingKey: Ordering[Key] = Ordering.by(v => v.value)
    implicit val orderingValue: Ordering[Value] = Ordering.by(v => -v.value)

    (for {
      storage <- Storage[IO, Key, Value]
      query <- StorageQuery(storage)

      _ <- storage.add(Key("Q1"), Value(123))
      _ <- storage.add(Key("Q2"), Value(231))
      _ <- storage.add(Key("Q3"), Value(456))
      _ <- storage.update(Key("Q2"), Value(777))
      _ <- storage.delete(Key("Q3"))
      lst <- query.getSorted

    } yield {
      lst.map(_._2.value) shouldEqual List(123, 777)
    }).unsafeRunSync()
  }
}
