package net.bmjames.opts

import scalaz.syntax.apply._

import net.bmjames.opts.types.Parser
import net.bmjames.opts.builder._
import net.bmjames.opts.extra._
import Parser._

case class Options(globalOpt: String, globalFlag: Boolean, command: Command)

sealed trait Command
case class Add(paths: List[String]) extends Command
case class Commit(message: String)  extends Command

object SubparserExample {

  val parseOpts: Parser[Options] =
    ^^(strOption(long("globalOpt"), help("Option that applies to all commands")),
       switch(long("globalFlag"),   help("Switch that applies to all commands")),
       subparser[Command](command("add",    info(many(strArgument(metavar("PATH"))).map(Add))),
                          command("commit", info(strArgument(metavar("MESSAGE")).map(Commit))))
    )(Options)

  def main(args: Array[String]) {
    val opts = info(parseOpts <*> helper, progDesc("A program with some global opts and command subparsers"))
    println(execParser(args, "SubparserExample", opts))
  }

}

/* Notes:
 * If you fail to provide the required --globalOpt option, but do provide the
 * required arguments to a subparser, the error message shows only the usage for the
 * subparser, and does not mention --globalOpt at all! This is confusing, however
 * optparse-applicative also appears to have the same behaviour (bug?).
 */
