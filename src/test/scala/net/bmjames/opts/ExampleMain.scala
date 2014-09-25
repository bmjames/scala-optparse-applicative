package net.bmjames.opts

import java.io.File

import scalaz.syntax.applicativePlus._

import builder._
import extra._
import types.Parser
import Parser._

case class Sample(hello: String, quiet: Boolean)

object SampleMain {

  val sample: Parser[Sample] =
    ^(
      strOption(long("hello"), metavar("TARGET"), help("Target for the greeting")),
      switch(long("quiet"), help("Whether to be quiet"))
    )(Sample.apply)

  def greet(s: Sample): Unit = s match {
    case Sample(h, false) => println("Hello, " ++ h)
    case _ =>
  }

  def main(args: Array[String]) {
    val opts = info(sample <*> helper,
      progDesc("Print a greeting for TARGET"),
      header("hello - a test for optparse-applicative"))
    greet(execParser(args, "SampleMain", opts))
  }

}
