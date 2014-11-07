version := "0.1-SNAPSHOT"

organization := "net.bmjames"

name := "scala-optparse-applicative"

scalaVersion in ThisBuild := "2.11.2"

crossScalaVersions := List("2.10.4", "2.11.2")

scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-language:existentials",
  "-language:higherKinds")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.7.0"
