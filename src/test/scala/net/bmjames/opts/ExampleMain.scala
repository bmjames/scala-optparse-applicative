package net.bmjames.opts

import java.io.File
import net.bmjames.opts.builder.internal.{OptionFields, FlagFields}

import scalaz.syntax.applicative._

import builder._
import extra._
import types.Parser

object ExampleMain {

  case class Opts(verbose: Boolean, input: File)

  val parseOpts: Parser[Opts] =
    ^(
      switch(short[FlagFields, Boolean]('v') <> long("verbose")),
      strOption(short[OptionFields, String]('f') <> long("file") <> metavar("FILE")).map(new File(_))
    )(Opts.apply)

  def main(args: Array[String]) {

    val opts = execParser(args.toList, "ExampleMain", info(parseOpts <*> helper, progDesc("An example program.")))

    println(opts)

  }

}
