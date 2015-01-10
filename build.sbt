name := "matasano"
organization := "ie.boboco"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
)

import scoverage.ScoverageSbtPlugin.ScoverageKeys._
coverageMinimum := 100
coverageFailOnMinimum := true