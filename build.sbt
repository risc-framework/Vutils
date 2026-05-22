ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version      := "0.1.0"
ThisBuild / organization := "risc.framework"
ThisBuild / publishTo    := Some(
  Resolver.file("local-ivy", file(Path.userHome + "/.ivy2/local"))
)

val hardfloatVersion = "1.5-SNAPSHOT"
val chiselVersion    = "7.0.0"

ThisBuild / scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Xcheckinit",
  "-Ymacro-annotations"
)

lazy val root = (project in file("."))
  .settings(
    name := "vutils",
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest" % "3.2.20" % Test,
      "org.chipsalliance" %% "chisel"    % chiselVersion,
      "edu.berkeley.cs"   %% "hardfloat" % hardfloatVersion,
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    ),
  )
