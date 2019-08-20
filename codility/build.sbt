import Dependencies._
import sbt._
import sbt.Keys._

lazy val compileFlags = Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)

lazy val codility = (project in file("."))
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq(scalaVersion.value, "2.11.11"),
    scalacOptions ++= compileFlags,
    organization := "com.maxkorolev",
    name := "codility",
    version := "0.0.1-SNAPSHOT",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4"),
    libraryDependencies ++= Seq(
      typesafeConfig,
      logbackClassic,
      scalaTest,
      catsEffect,
      shapeless,
      simulacrum,
      fastparse
    )
  )
