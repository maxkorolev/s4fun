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
    val httpApp = (
      BankRoutes.accountRoutes[F]
    ).orNotFound

    // With Middlewares in place
    val finalHttpApp = Logger.httpApp(true, true)(httpApp)

    BlazeServerBuilder[F]
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(finalHttpApp)
      .serve
  }.drain
}
