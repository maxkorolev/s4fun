package bank

import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import cats.implicits._
import fs2.Stream
import fs2.concurrent._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import scala.concurrent.ExecutionContext.global
import pureconfig.generic.auto._

object BankServer {

  def stream[F[_]: ConcurrentEffect](
      implicit T: Timer[F],
      C: ContextShift[F]
  ): Stream[F, Nothing] = {

    val config = pureconfig.loadConfigOrThrow[BankConfig]("bank")
    for {
      topic <- Stream.eval(Topic[F, Bank.Transaction](Bank.Init))
      vault <- Stream.eval(Storage[F, Bank.UserID, Bank.Money])
      bank = Bank.impl[F](topic)

      httpApp = (
        BankRoutes.accountRoutes[F]
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode

  }.drain
}
