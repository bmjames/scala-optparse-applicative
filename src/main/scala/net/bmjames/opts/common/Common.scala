package net.bmjames.opts.common

import net.bmjames.opts.internal._
import net.bmjames.opts.types._

import scalaz._
import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.monadPlus._

private[opts] trait Common {

  def showOption(name: OptName): String =
    name match {
      case OptLong(n)  => s"--$n"
      case OptShort(n) => s"-$n"
    }

  def argMatches[F[_], A](opt: OptReader[A], arg: String)(implicit F: MonadP[F]): Option[StateT[F, Args, A]] =
    opt match {
      case ArgReader(rdr) =>
        Some(runReadM(rdr.reader, arg).liftM[({type λ[μ[_],α]=StateT[μ,Args,α]})#λ])
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
          missingArg: StateT[F, Args, (String, Args)] = F.missingArg(noArgErr).liftM[({type λ[μ[_],α]=StateT[μ,Args,α]})#λ]
          as <- mbArgs.fold(missingArg)(_.point[({type λ[α]=StateT[F,Args,α]})#λ])
          (arg1, args1) = as
          _ <- state.put(args1)
          run <- rdr.reader.run.run(arg1).fold(
            e => errorFor(word.name, e).liftM[({type λ[μ[_],α]=StateT[μ,Args,α]})#λ],
            r => r.point[({type λ[α]=StateT[F,Args,α]})#λ])
        } yield run
        Some(read)
      case FlagReader(names, x) if hasName(word.name, names) && word.value.isEmpty =>
        Some(x.point[({type λ[α]=StateT[F,Args,α]})#λ])
      case _ => None
    }
  }

  def isArg[A](r: OptReader[A]): Boolean =
    r match {
      case ArgReader(_) => true
      case _            => false
    }

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
          x  <- evalParser(p1).orEmpty[({type λ[α]=NondetT[F,α]})#λ]
        } yield k(x)
    }

  /** The default value of a Parser. This function returns an error if any of the options don't have a default value
    */
  def evalParser[A](p: Parser[A]): Option[A] =
    p match {
      case NilP(r)       => r
      case OptP(_)       => None
      case MultP(p1, p2) => evalParser(p2) <*> evalParser(p1)
      case AltP(p1, p2)  => evalParser(p1) <+> evalParser(p2)
      case BindP(p, k)   => evalParser(p) >>= k.andThen(evalParser[A])
    }

  /** Map a polymorphic function over all the options of a parser, and collect the results in a list.
    */
  def mapParser[A, B](f: OptHelpInfo => (Opt ~> ({type λ[α]=Const[B,α]})#λ), p: Parser[A]): List[B] = {
    def flatten[AA](t: OptTree[AA]): List[AA] =
      t match {
        case Leaf(x)      => List(x)
        case MultNode(xs) => xs.flatMap(flatten)
        case AltNode(xs)  => xs.flatMap(flatten)
      }
    flatten(treeMapParser(f, p))
  }

  /** Like mapParser, but collect the results in a tree structure.
    */
  def treeMapParser[A, B](g: OptHelpInfo => (Opt ~> ({type λ[α]=Const[B,α]})#λ), p: Parser[A]): OptTree[B] = {
    def hasDefault[AA](p: Parser[AA]): Boolean =
      evalParser(p).isDefined

    def go[AA](m: Boolean, d: Boolean, f: OptHelpInfo => (Opt ~> ({type λ[α]=Const[B,α]})#λ), p: Parser[AA]): OptTree[B] =
      p match {
        case NilP(_) => MultNode(Nil)
        case OptP(opt) if opt.props.visibility > Internal => Leaf(f(OptHelpInfo(m, d))(opt).getConst)
        case OptP(opt) => MultNode(Nil)
        case MultP(p1, p2) => MultNode(List(go(m, d, f, p1), go(m, d, f, p2)))
        case AltP(p1, p2) =>
          val d1 = d || hasDefault(p1) || hasDefault(p2)
          AltNode(List(go(m, d1, f, p1), go(m, d1, f, p2)))
        case BindP(p, _) => go(true, d, f, p)
      }

    simplify(go(false, false, g, p))
  }

  def simplify[A](as: OptTree[A]): OptTree[A] = {
    def removeMult[AA](as: OptTree[AA]): List[OptTree[AA]] =
      as match {
        case MultNode(ts) => ts
        case t => List(t)
      }

    def removeAlt[AA](as: OptTree[AA]): List[OptTree[AA]] =
      as match {
        case AltNode(ts)   => ts
        case MultNode(Nil) => Nil
        case t             => List(t)
      }

    as match {
      case Leaf(x) => as
      case MultNode(xs) => xs.flatMap(x => removeMult(simplify(x))) match {
        case List(x) => x
        case xs      => MultNode(xs)
      }
      case AltNode(xs) => xs.flatMap(x => removeAlt(simplify(x))) match {
        case Nil     => MultNode(Nil)
        case List(x) => x
        case xs      => AltNode(xs)
      }
    }
  }

  def isOptionPrefix(n1: OptName, n2: OptName): Boolean =
    (n1, n2) match {
      case (OptShort(x), OptShort(y)) => x == y
      case (OptLong(x),  OptLong(y))  => y.startsWith(x)
      case _                          => false
    }

  /** Create a parser composed of a single operation. */
  def liftOpt[A](opt: Opt[A]): Parser[A] = OptP(opt)

  trait ArgsState[F[_]] {
    type G[A] = StateT[F, Args, A]
  }

  def searchOpt[F[_]: MonadP, A](pprefs: ParserPrefs, w: OptWord, p: Parser[A]): NondetT[ArgsState[F]#G, Parser[A]] = {
    val f = new (Opt ~> ({type λ[α]=NondetT[ArgsState[F]#G,α]})#λ) {
      def apply[AA](fa: Opt[AA]): NondetT[ArgsState[F]#G, AA] = {
        val disambiguate = pprefs.disambiguate && fa.props.visibility > Internal
        optMatches(disambiguate, fa.main, w) match {
          case Some(matcher) => matcher.liftM[({type λ[μ[_],α]=NondetT[μ,α]})#λ]
          case None          => NondetT.empty[ArgsState[F]#G, AA]
        }
      }
    }
    searchParser[ArgsState[F]#G, A](f, p)
  }

  import NondetT._

  def searchArg[F[_]: MonadP, A](arg: String, p: Parser[A]): NondetT[ArgsState[F]#G, Parser[A]] = {
    val f = new (Opt ~> ({type λ[α]=NondetT[ArgsState[F]#G,α]})#λ) {
      def apply[AA](fa: Opt[AA]): NondetT[ArgsState[F]#G, AA] =
        (if (isArg(fa.main)) cut[ArgsState[F]#G] else NondetT.pure[ArgsState[F]#G, Unit](())).flatMap(
          p => argMatches[F, AA](fa.main, arg) match {
            case Some(matcher) => matcher.liftM[({type λ[μ[_],α]=NondetT[μ,α]})#λ]
            case None          => NondetT.empty[ArgsState[F]#G, AA]
          }
        )
    }
    searchParser[ArgsState[F]#G, A](f, p)
  }

  def stepParser[F[_]: MonadP, A](pprefs: ParserPrefs,
                                  policy: ArgPolicy,
                                  arg: String,
                                  p: Parser[A]): NondetT[ArgsState[F]#G, Parser[A]] =
    policy match {
      case SkipOpts => parseWord(arg) match {
        case Some(w) => searchOpt(pprefs, w, p)
        case None    => searchArg(arg, p)
      }
      case AllowOpts =>
        val p1: NondetT[ArgsState[F]#G, Parser[A]] = searchArg[F, A](arg, p)
        val ev = NondetT.nondetTMonadPlus[ArgsState[F]#G]
        val w = parseWord(arg).orEmpty[({type λ[α]=NondetT[ArgsState[F]#G, α]})#λ](ev, ev)
        val p2 = w.flatMap(searchOpt[F, A](pprefs, _, p))
        p1 orElse p2
    }

  /** Apply a Parser to a command line, and return a result and leftover arguments.
    * This function returns an error if any parsing error occurs, or if any options are missing and don't have a default value.
    */
  def runParser[F[_], A](policy: ArgPolicy, p: Parser[A], args: Args)(implicit F: MonadP[F]): F[(Args, A)] = {
    lazy val result = evalParser(p).map(args -> _)

    def doStep(prefs: ParserPrefs, arg: String, argt: Args): F[(Args, Option[Parser[A]])] =
      disamb[ArgsState[F]#G, Parser[A]](! prefs.disambiguate, stepParser(prefs, policy, arg, p)).run(argt)

    (policy, args) match {
      case (SkipOpts, "--" :: argt) => runParser(AllowOpts, p, argt)
      case (_, Nil) => F.exit(p, result)
      case (_, arg :: argt) =>
        for {
          prefs <- F.getPrefs
          s     <- doStep(prefs, arg, argt)
          (args1, mp) = s
          run <- mp match {
            case None => result.orEmpty[F] <+> parseError(arg)
            case Some(p1) => runParser(policy, p1, args1)
          }
        } yield run
    }
  }

  def parseError[F[_], A](arg: String)(implicit F: MonadP[F]): F[A] = {
    val msg = if (arg.startsWith("-")) s"Invalid option `$arg'"
              else s"Invalid argument `$arg'"
    F.error(ErrorMsg(msg))
  }

  def getPolicy[A](i: ParserInfo[A]): ArgPolicy =
    if (i.intersperse) SkipOpts else AllowOpts

  def runParserInfo[F[_]: MonadP, A](i: ParserInfo[A], args: Args): F[A] =
    runParserFully(getPolicy(i), i.parser, args)

  def runParserFully[F[_]: MonadP, A](policy: ArgPolicy, p: Parser[A], args: Args): F[A] =
    for ((Nil, r) <- runParser(policy, p, args)) yield r

}
