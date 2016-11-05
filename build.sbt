version := "0.4"

organization := "net.bmjames"

name := "scala-optparse-applicative"

scalaVersion := "2.11.8"

crossScalaVersions := List("2.10.6", "2.11.8", "2.12.0")

scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-language:existentials",
  "-language:higherKinds")

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.2.7",
  "com.googlecode.kiama" %% "kiama" % "1.8.0",
  "org.scalacheck" %% "scalacheck" % "1.12.6" % "test")
