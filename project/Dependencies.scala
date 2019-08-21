import sbt._

object Dependencies {
  val Http4sVersion = "0.20.1"
  val CirceVersion = "0.11.1"
  val Specs2Version = "4.1.0"
  val LogbackVersion = "1.2.3"

  lazy val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.2.3"
  lazy val typesafeConfig = "com.typesafe" % "config" % "1.3.2"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % Test

  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.2.0"

  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
  lazy val simulacrum = "com.github.mpilquist" %% "simulacrum" % "0.15.0"
  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "2.1.0"
  lazy val pureconfig = "com.github.pureconfig" %% "pureconfig" % "0.11.0"
  lazy val sangria = "org.sangria-graphql" %% "sangria" % "1.4.2"

  lazy val http4s = Seq(
    "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
    "org.http4s" %% "http4s-circe" % Http4sVersion,
    "org.http4s" %% "http4s-dsl" % Http4sVersion,
    "io.circe" %% "circe-generic" % CirceVersion
  )

}
