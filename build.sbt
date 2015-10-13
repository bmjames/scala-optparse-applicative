version := "0.3"

organization := "net.bmjames"

name := "scala-optparse-applicative"

scalaVersion := "2.11.7"

crossScalaVersions := List("2.10.6", "2.11.7")

scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-language:existentials",
  "-language:higherKinds")

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.1.4",
  "com.googlecode.kiama" %% "kiama" % "1.7.0",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test")
