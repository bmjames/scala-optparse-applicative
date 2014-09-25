package net.bmjames.opts

import java.io.File

import scalaz.syntax.applicativePlus._

import builder._
import extra._
import types.Parser
import Parser._

object ExampleMain {

  case class Opts(verbose: Boolean, inputs: List[File], output: Option[File])

  val parseOpts: Parser[Opts] =
    ^^(
      switch(short('v'), long("verbose")),
      some(strArgument(metavar("FILE"), help("Files to read")).map(new File(_))),
      optional(strOption(short('f'), long("file"), metavar("FILE")).map(new File(_)))
    )(Opts.apply)

  def main(args: Array[String]) {
    val opts = execParser(args, "ExampleMain", info(parseOpts <*> helper, progDesc("An example program.")))
    println(opts)
  }

}
