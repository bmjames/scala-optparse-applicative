package net.bmjames.opts

import java.io.File

import scalaz.syntax.applicativePlus._

import builder._
import extra._
import types.Parser
import Parser.pure

object ExampleMain {

  case class Opts(verbose: Boolean, input: File)

  val parseOpts: Parser[Opts] =
    ^(
      switch    (short('v'), long("verbose")),
      strOption (short('f'), long("file"), metavar("FILE")).map(new File(_)) <|> pure(new File("/tmp/default"))
    )(Opts.apply)

  def main(args: Array[String]) {
    val opts = execParser(args, "ExampleMain", info(parseOpts <*> helper, progDesc("An example program.")))
    println(opts)
  }

}
