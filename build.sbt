version := "0.2"

organization := "net.bmjames"

name := "scala-optparse-applicative"

scalaVersion := "2.11.4"

crossScalaVersions := List("2.10.4", "2.11.4")

scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-language:existentials",
  "-language:higherKinds")

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "com.googlecode.kiama" %% "kiama" % "1.7.0",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test")
