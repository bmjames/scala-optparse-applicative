package net.bmjames.opts

import net.bmjames.opts.internal.{P, NondetT, uncons, MonadP}
import net.bmjames.opts.types._

import scalaz._
import scalaz.syntax.monadPlus._
import net.bmjames.opts.types.CmdReader
import net.bmjames.opts.types.OptP
import net.bmjames.opts.types.NilP
import net.bmjames.opts.types.AltP
import scala.Some
import net.bmjames.opts.types.ErrorMsg
import net.bmjames.opts.types.OptShort
import net.bmjames.opts.types.OptLong
import net.bmjames.opts.types.FlagReader
import net.bmjames.opts.types.ArgReader
import net.bmjames.opts.types.MultP
import net.bmjames.opts.types.OptionReader
import net.bmjames.opts.types.BindP

package object common {

  def showOption(name: OptName): String =
    name match {
      case OptLong(n)  => s"--$n"
      case OptShort(n) => s"-$n"
    }

  def argMatches[F[_], A](opt: OptReader[A], arg: String)(implicit F: MonadP[F]): Option[StateT[F, Args, A]] =
    opt match {
      case ArgReader(rdr) =>
        rdr.run(arg).map(_.point[({type λ[α]=StateT[F,Args,α]})#λ])
      case CmdReader(_, f) =>
        f(arg).map { subp =>
          StateT[F, Args, A] { args =>
            for {
              _      <- F.setContext(Some(arg), subp)
              prefs  <- F.getPrefs
              runSub <- if (prefs.backtrack)
                          runParser(getPolicy(subp), subp.parser, args)
                        else
                          runParserInfo(subp, args).map(Nil -> _)
            } yield runSub
          }
        }
      case _ => None
    }

  private def argsMState[F[_]: Monad] = MonadState[({type λ[α,β]=StateT[F,α,β]})#λ, Args]

  def optMatches[F[_], A](disambiguate: Boolean, opt: OptReader[A], word: OptWord)(implicit F: MonadP[F]): Option[StateT[F, Args, A]] = {
    def hasName(n: OptName, ns: List[OptName]): Boolean =
      if (disambiguate) ns.exists(isOptionPrefix(n, _)) else ns.contains(n)

    def errorFor(name: OptName, e: ParseError) =
      e match {
        case ErrorMsg(msg) => F.error(ErrorMsg(s"option ${showOption(name)}: $msg"))
        case _             => F.error(e)
      }

    val state = argsMState[F]
    opt match {
      case OptionReader(names, rdr, noArgErr) if hasName(word.name, names) =>
        val read: StateT[F, Args, A] = for {
          args <- state.get
          mbArgs = uncons(word.value.toList ++ args)
          missingArg: StateT[F, Args, (String, Args)] = F.missingArg(noArgErr).liftM[({type λ[ζ[_],α]=StateT[ζ,Args,α]})#λ]
          as <- mbArgs.fold(missingArg)(_.point[({type λ[α]=StateT[F,Args,α]})#λ])
          (arg1, args1) = as
          _ <- state.put(args1)
          run <- rdr.run(arg1).run.fold(
            e => errorFor(word.name, e).liftM[({type λ[ζ[_],α]=StateT[ζ,Args,α]})#λ],
            r => r.point[({type λ[α]=StateT[F,Args,α]})#λ])
        } yield run
        Some(read)
      case FlagReader(names, x) if hasName(word.name, names) && word.value.isEmpty =>
        Some(x.point[({type λ[α]=StateT[F,Args,α]})#λ])
      case _ => None
    }
  }

  final case class OptWord(name: OptName, value: Option[String])

  def parseWord(s: String): Option[OptWord] =
    if (s.startsWith("--")) {
      val w = s.drop(2)
      val (opt, arg) = w.span(_ != '=') match {
        case (_, "")  => (w, None)
        case (w1, w2) => (w1, Some(w2.tail))
      }
      Some(OptWord(OptLong(opt), arg))
    }
    else if (s.startsWith("-")) {
      s.drop(1) match {
        case "" => None
        case w  =>
          val (a, rest) = w.splitAt(1)
          val arg = Some(rest).filter(_.nonEmpty)
          Some(OptWord(OptShort(a.head), arg))
      }
    }
    else None

  def searchParser[F[_]: Monad, A](f: Opt ~> ({type λ[α]=NondetT[F,α]})#λ, p: Parser[A]): NondetT[F, Parser[A]] =
    p match {
      case NilP(_)   => PlusEmpty[({type λ[α]=NondetT[F,α]})#λ].empty
      case OptP(opt) => f(opt).map(_.point[Parser])
      case MultP(p1, p2) =>
        searchParser(f, p1).map(p2 <*> _) ! searchParser(f, p2).map(_ <*> p1)
      case AltP(p1, p2) =>
        searchParser(f, p1) <+> searchParser(f, p2)
      case bindP @ BindP(p, k) =>
        for {
          p1 <- searchParser(f, p)
          x  <- P.hoistMaybe[({type λ[α]=NondetT[F,α]})#λ, bindP.X](evalParser(p1))
        } yield k(x)
    }

  def evalParser[A](p: Parser[A]): Option[A] =
    ???

  def isOptionPrefix(n1: OptName, n2: OptName): Boolean =
    (n1, n2) match {
      case (OptShort(x), OptShort(y)) => x == y
      case (OptLong(x),  OptLong(y))  => y.startsWith(x)
      case _                          => false
    }

  def runParser[F[_]: MonadP, A](policy: ArgPolicy, p: Parser[A], args: Args): F[(Args, A)] =
    ???

  def getPolicy[A](i: ParserInfo[A]): ArgPolicy =
    ???

  def runParserInfo[F[_]: MonadP, A](i: ParserInfo[A], args: Args): F[A] =
    ???

  def runParserFully[F[_]: MonadP, A](policy: ArgPolicy, p: Parser[A], args: Args): F[A] =
    ???

}
