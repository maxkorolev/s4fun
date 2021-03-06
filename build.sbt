import sbt._
import sbt.Keys._
import Dependencies._

resolvers in ThisBuild += Resolver.sonatypeRepo("snapshots")
resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

lazy val s4fun =
  (project in file(".")).aggregate(mini_book, animation, bank, recursion)

lazy val mini_book = project
lazy val animation = project
lazy val bank = project
lazy val recursion = project
lazy val codility = project
