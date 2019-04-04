import sbt._
import sbt.Keys._
import Dependencies._

resolvers in ThisBuild += Resolver.sonatypeRepo("snapshots")
resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

lazy val s4fun = (project in file(".")).aggregate(mini_book, animation)

lazy val mini_book = project
lazy val animation = project
