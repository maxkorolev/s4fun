import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % Test

  lazy val logbackClassic      = "ch.qos.logback"              % "logback-classic"       % "1.2.3"
  lazy val typesafeConfig      = "com.typesafe"                % "config"                % "1.3.2"
  
  lazy val cats                = "org.typelevel"          %% "cats-core"             % "1.1.0"

  lazy val shapeless           = "com.chuusai"            %% "shapeless"             % "2.3.3"
}
