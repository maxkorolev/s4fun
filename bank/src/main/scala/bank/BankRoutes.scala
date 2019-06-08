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

  def accountRoutes[F[_]: Sync]: HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case req @ GET -> Root / "balance" =>
        Ok("")
      case req @ POST -> Root / "deposit" =>
        Ok("")
      case req @ POST -> Root / "withdrawal" =>
        Ok("")

    }
  }

}
