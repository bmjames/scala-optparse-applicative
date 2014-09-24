package net.bmjames.opts

import net.bmjames.opts.builder._
import net.bmjames.opts.common._
import net.bmjames.opts.helpdoc._
import net.bmjames.opts.internal._
import net.bmjames.opts.types._
import net.bmjames.opts.builder.internal.OptionFields

import scalaz.{Const, ~>, -\/, \/-}
import scalaz.syntax.applicative._
import scalaz.syntax.semigroup._

import org.kiama.output.{PrettyPrinter => PP}

package object extra {

  /** A hidden "helper" option which always fails */
  def helper[A]: Parser[A => A] =
    abortOption(ShowHelpText, long[OptionFields, A => A]("help") <> short('h') <> help("Show this help text") <> hidden)

  def execParser[A](args: Array[String], progName: String, pinfo: ParserInfo[A]): A =
    customExecParser(args.toList, progName, prefs(idm[PrefsMod]), pinfo)

  def customExecParser[A](args: List[String], progName: String, pprefs: ParserPrefs, pinfo: ParserInfo[A]): A =
    handleParseResult(progName, execParserPure(pprefs, pinfo, args))

  def handleParseResult[A](progName: String, result: ParserResult[A]): A =
    result match {
      case Success(a) => a
      case Failure(f) =>
        val (msg, exit) = renderFailure(f, progName)
        exit match {
          case ExitSuccess => println(msg)
          case _           => Console.err.println(msg)
        }
        sys.exit(exit.toInt)
    }

  def execParserPure[A](pprefs: ParserPrefs, pinfo: ParserInfo[A], args: List[String]): ParserResult[A] = {
    val p = runParserInfo[P, A](pinfo, args)
    runP(p, pprefs) match {
      case (_, \/-(r))     => Success(r)
      case (ctx, -\/(err)) => Failure(parserFailure(pprefs, pinfo, err, ctx))
    }
  }

  /** Generate a ParserFailure from a ParseError in a given Context. */
  def parserFailure[A](pprefs: ParserPrefs, pinfo: ParserInfo[A], msg: ParseError, ctx: Context): ParserFailure[ParserHelp] =
    ParserFailure { progName =>
      val exitCode = msg match {
        case ErrorMsg(_) | UnknownError => ExitFailure(pinfo.failureCode)
        case _ => ExitSuccess
      }

      def withContext[A, B](ctx: Context, pinfo: ParserInfo[A], f: List[String] => ParserInfo ~> (({type λ[α]=Const[B,α]})#λ)): B =
        ctx match {
          case NullContext      => f(Nil)(pinfo).getConst
          case HasContext(n, i) => f(n)(i).getConst
        }

      def usage_help[A](progName: String, names: List[String], i: ParserInfo[A]): ParserHelp =
        msg match {
          case InfoMsg (_) => ParserHelp.empty
          case _ => usageHelp(Chunk.vcatChunks(List(
            parserUsage(pprefs, i.parser, unwords(progName :: names)).pure[Chunk],
            i.progDesc.map(PP.indent(_, 2))
          )))
        }

      val errorHelp: ParserHelp =
        helpdoc.errorHelp(msg match {
          case ShowHelpText => Chunk.empty
          case ErrorMsg(m)  => Chunk.fromString(m)
          case InfoMsg(m)   => Chunk.fromString(m)
          case UnknownError => Chunk.empty
        })

      val showFullHelp = msg match {
        case ShowHelpText => true
        case _            => pprefs.showHelpOnError
      }

      def baseHelp[A](i: ParserInfo[A]): ParserHelp =
        if (showFullHelp) h |+| footerHelp(i.header) |+| parserHelp(pprefs, i.parser)
        else ParserHelp.empty

      def h = withContext[A, ParserHelp](ctx, pinfo, names => new (ParserInfo ~> (({type λ[α]=Const[ParserHelp,α]})#λ)) {
        def apply[A](fa: ParserInfo[A]): Const[ParserHelp, A] = Const {
          baseHelp(fa) |+| usage_help(progName, names, fa) |+| errorHelp
        }
      })

      (h, exitCode, pprefs.columns)
    }

  def renderFailure(failure: ParserFailure[ParserHelp], progName: String): (String, ExitCode) = {
    val (h, exit, cols) = failure.run(progName)
    (ParserHelp.renderHelp(cols, h), exit)
  }

}
