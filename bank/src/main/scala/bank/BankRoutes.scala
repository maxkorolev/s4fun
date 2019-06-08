package bank

import io.circe._
import cats.effect.Sync
import cats.syntax.all._
import cats.Applicative
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import shapeless._

object BankRoutes {

  implicit def entityDecoder[F[_]: Sync, T: Decoder](
      implicit notEq: T =:!= Json
  ): EntityDecoder[F, T] =
    jsonOf[F, T]

  implicit def entityEncoder[F[_]: Applicative, T: Encoder](
      implicit notEq: T =:!= Json
  ): EntityEncoder[F, T] =
    jsonEncoderOf[F, T]

  def accountRoutes[F[_]: Sync](
      bank: Bank[F],
      vault: Storage[F, Bank.UserID, Bank.Money]
  ): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "balance" / userID =>
        vault.get(userID) flatMap {
          case Some(value) => Ok(value)
          case None        => NotFound("")
        }
      case req @ POST -> Root / "deposit" / userID =>
        for {
          dto <- req.as[Bank.AmountDTO]
          _ <- bank.deposit(userID, dto.amount)
          resp <- Ok("")
        } yield resp
      case req @ POST -> Root / "withdraw" / userID =>
        for {
          dto <- req.as[Bank.AmountDTO]
          _ <- bank.withdraw(userID, dto.amount)
          resp <- Ok("")
        } yield resp

    }
  }

}
