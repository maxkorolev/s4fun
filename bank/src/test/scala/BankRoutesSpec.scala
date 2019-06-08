package bank

import io.circe.syntax._
import fs2._
import fs2.concurrent._
import cats.effect.IO
import org.http4s._
import org.http4s.circe._
import org.http4s.implicits._
import java.{util => ju}

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class BankRoutesSpec extends FlatSpec with Matchers {

  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  "BankRoutes" should "handle requests" in {

    val depositReq = Request[IO](Method.POST, uri"/deposit/maxkorolev")
      .withEntity(Bank.AmountDTO(200).asJson)
    val withdrawalReq = Request[IO](Method.POST, uri"/withdraw/maxkorolev")
      .withEntity(Bank.AmountDTO(50).asJson)
    val balanceReq = Request[IO](Method.GET, uri"/balance/maxkorolev")

    (for {
      topic <- Stream.eval(Topic[IO, Bank.Transaction](Bank.Init))
      vault <- Stream.eval(Storage[IO, Bank.UserID, Bank.Money])
      bank = Bank.impl[IO](topic)
      accounting = Accounting.impl(topic, vault).run

      routes = BankRoutes.accountRoutes[IO](bank, vault)

      deposit <- Stream eval routes.orNotFound(depositReq)
      _ <- accounting.take(1)

      withdrawal <- Stream eval routes.orNotFound(withdrawalReq)
      _ <- accounting.take(1)

      balance <- Stream eval routes.orNotFound(balanceReq)
      res <- Stream eval balance.as[String].map(BigDecimal.apply)

    } yield {
      withdrawal.status shouldEqual Status.Ok
      deposit.status shouldEqual Status.Ok
      res shouldEqual 150
    }).compile.drain.unsafeRunSync()
  }
}
