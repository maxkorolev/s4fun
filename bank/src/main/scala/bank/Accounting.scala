package bank

import cats.Monad
import cats.syntax.all._
import cats.effect._
import io.circe._
import io.circe.generic.semiauto._
import java.time.Instant
import fs2._
import fs2.concurrent._
import java.{util => ju}
import bank.Bank.Deposit
import bank.Bank.Withdrawal

trait Accounting[F[_]] {

  def run: Stream[F, Bank.Transaction]
}

object Accounting {

  def impl[F[_]](
      eventsTopic: Topic[F, Bank.Transaction],
      vault: Storage[F, Bank.UserID, Bank.Money]
  )(implicit F: Sync[F]): Accounting[F] =
    new Accounting[F] {

      def run: Stream[F, Bank.Transaction] = {
        eventsTopic.subscribe(1).evalMap[F, Bank.Transaction] {
          case d: Deposit => addOrUpdate(vault, d.userID, d.amount).map(_ => d)
          case w: Withdrawal =>
            addOrUpdate(vault, w.userID, -w.amount).map(_ => w)
          case trans: Bank.Transaction => F.pure(trans)
        }
      }
    }

  private def addOrUpdate[F[_]: Monad](
      vault: Storage[F, Bank.UserID, Bank.Money],
      userID: Bank.UserID,
      amount: Bank.Money
  ): F[Unit] =
    vault.get(userID) >>= {
      case Some(_) => vault.updateValue(userID)(_ + amount)
      case None    => vault.add(userID, amount)
    }
}
