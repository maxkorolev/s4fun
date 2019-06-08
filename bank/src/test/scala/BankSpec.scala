package bank

import fs2._
import fs2.concurrent._
import cats.effect._
import org.http4s._
import org.http4s.implicits._
import scala.concurrent.ExecutionContext
import java.{util => ju}

import org.scalatest._
import java.time.Instant
import bank.Bank.Deposit

class BankSpec extends FlatSpec with Matchers {

  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  "BankRoutes" should "handle requests" in {

    val io = for {
      topic <- Stream.eval(Topic[IO, Bank.Transaction](Bank.Init))
      service = Bank.impl[IO](topic)
      userID = "maxkorolev"
      amount = 100

      publishDeposit = Stream.eval(service.deposit(userID, amount))
      publishWithdrawal = Stream.eval(
        service.withdraw(userID, amount)
      )

      (transID, trans) <- (publishDeposit ++ publishWithdrawal) zip service.transactions

    } yield {
      trans match {
        case Bank.Deposit(id, _, _, _)    => id == transID
        case Bank.Withdrawal(id, _, _, _) => id == transID
        case _                            => false
      }
    }

    io.compile.toList.unsafeRunSync() shouldEqual List(true, true)

  }
}
