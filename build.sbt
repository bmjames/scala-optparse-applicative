version := "0.7"

organization := "net.bmjames"

name := "scala-optparse-applicative"

scalaVersion := "2.11.12"

crossScalaVersions := List("2.10.7", "2.11.12", "2.12.4")

scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-language:existentials",
  "-language:higherKinds")

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.2.20",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test")

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.6" cross CrossVersion.binary)
