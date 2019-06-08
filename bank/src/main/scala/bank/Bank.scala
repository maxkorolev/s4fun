package bank

import cats.syntax.all._
import cats.effect._
import io.circe._
import io.circe.generic.semiauto._
import java.time.Instant
import fs2._
import fs2.concurrent._
import java.{util => ju}

trait Bank[F[_]] {

  def withdraw(userID: Bank.UserID, amount: Bank.Money): F[Bank.TransactionID]

  def deposit(userID: Bank.UserID, amount: Bank.Money): F[Bank.TransactionID]

  def transactions: Stream[F, Bank.Transaction]
  def withdrawals: Stream[F, Bank.Withdrawal]
  def deposits: Stream[F, Bank.Deposit]

}

object Bank {

  type TransactionID = ju.UUID
  type UserID = String
  type Money = BigDecimal
  type Day = Int
  type CreatedAt = Instant

  sealed trait Transaction

  final case object Init extends Transaction
  final case class Withdrawal(
      id: TransactionID,
      userID: UserID,
      amount: Money,
      createdAt: CreatedAt
  ) extends Transaction
  object Withdrawal {
    implicit val decoder: Decoder[Withdrawal] = deriveDecoder
    implicit val encoder: Encoder[Withdrawal] = deriveEncoder
  }

  final case class Deposit(
      id: TransactionID,
      userID: UserID,
      amount: Money,
      createdAt: CreatedAt
  ) extends Transaction
  object Deposit {
    implicit val decoder: Decoder[Deposit] = deriveDecoder
    implicit val encoder: Encoder[Deposit] = deriveEncoder
  }

  def impl[F[_]](
      eventsTopic: Topic[F, Transaction]
  )(implicit F: Sync[F]): Bank[F] =
    new Bank[F] {

      def withdraw(
          userID: Bank.UserID,
          amount: Bank.Money
      ): F[Bank.TransactionID] =
        for {
          id <- F.delay(ju.UUID.randomUUID())
          createdAt <- F.delay(Instant.now)
          trans = Withdrawal(id, userID, amount, createdAt)
          _ <- eventsTopic.publish1(trans)
        } yield id

      def deposit(
          userID: Bank.UserID,
          amount: Bank.Money
      ): F[Bank.TransactionID] =
        for {
          id <- F.delay(ju.UUID.randomUUID())
          createdAt <- F.delay(Instant.now)
          trans = Deposit(id, userID, amount, createdAt)
          _ <- eventsTopic.publish1(trans)
        } yield id

      def transactions: Stream[F, Bank.Transaction] =
        eventsTopic.subscribe(1)

      def withdrawals: Stream[F, Bank.Withdrawal] =
        eventsTopic.subscribe(1).flatMap {
          case e: Bank.Withdrawal => Stream.emit(e)
          case _                  => Stream.empty
        }

      def deposits: Stream[F, Bank.Deposit] =
        eventsTopic.subscribe(1).flatMap {
          case e: Bank.Deposit => Stream.emit(e)
          case _               => Stream.empty
        }
    }
}
