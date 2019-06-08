package bank

import fs2._
import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import java.{util => ju}

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext

class BankRoutesSpec extends FlatSpec with Matchers {

  // implicit val contextShift: ContextShift[IO] =
  // IO.contextShift(ExecutionContext.global)

  private[this] val routes = BankRoutes.accountRoutes[IO]

  "BankRoutes" should "handle requests" in {

    val withdrawalReq = Request[IO](Method.POST, uri"/withdrawal")
    val depositReq = Request[IO](Method.POST, uri"/deposit")
    val balanceReq = Request[IO](Method.GET, uri"/balance")

    (for {
      withdrawal <- routes.orNotFound(withdrawalReq)
      deposit <- routes.orNotFound(depositReq)
      balance <- routes.orNotFound(balanceReq)

    } yield {
      withdrawal.status shouldEqual Status.Ok
      deposit.status shouldEqual Status.Ok
      balance.status shouldEqual Status.Ok
    }).unsafeRunSync()
  }
}
