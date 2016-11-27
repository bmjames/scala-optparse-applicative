package net.bmjames.opts.helpdoc

import net.bmjames.opts.types._
import net.bmjames.opts.common.showOption

import scalaz._
import scalaz.std.option._
import scalaz.syntax.std.list._
import scalaz.syntax.std.boolean._
import scalaz.syntax.functor._
import scalaz.syntax.monoid._


private[opts] trait Help {

  import Chunk._

  /** Generate description for a single option. */
  def optDesc[A](pprefs: ParserPrefs, style: OptDescStyle, info: OptHelpInfo, opt: Opt[A]): Chunk[Doc] = {
    val ns = opt.main.names
    val mv = Chunk.fromString(opt.props.metaVar)
    val descs = ns.sorted.map(Doc.string _ compose showOption)
    val desc = Chunk.fromList(descs.intersperse(style.sep)) <<+>> mv
    val vis = opt.props.visibility
    val showOpt = if (vis == Hidden) style.hidden else vis == Visible
    val suffix: Chunk[Doc] = if (info.multi) Chunk.fromString(pprefs.multiSuffix) else Chunk.empty

    def render(chunk: Chunk[Doc]): Chunk[Doc] =
      if (! showOpt) Chunk.empty
      else if (chunk.isEmpty || ! style.surround) chunk <> suffix
      else if (info.default) chunk.map(_.brackets) <> suffix
      else if (descs.drop(1).isEmpty) chunk <> suffix
      else chunk.map(_.parens) <> suffix

    render(desc)
  }

  /** Generate descriptions for commands. */
  def cmdDesc[A](p: Parser[A]): Chunk[Doc] =
    Chunk.vcatChunks(p.mapPoly(_ => new (Opt ~> ({type λ[α]=Const[Chunk[Doc],α]})#λ) {
      def apply[AA](fa: Opt[AA]): Const[Chunk[Doc], AA] =
        Const(fa.main match {
          case CmdReader(cmds, p) =>
            Chunk.tabulate(
              for (cmd <- cmds.reverse; d <- p(cmd).map(_.progDesc).toList)
              yield (Doc.string(cmd), extract(d)) //TODO Colt: Check alignment
            )
          case _ => Chunk.empty
        })
    }))

  /** Generate a brief help text for a parser. */
  def briefDesc[A](pprefs: ParserPrefs, parser: Parser[A]): Chunk[Doc] = {
    val style = OptDescStyle(sep = Doc.string("|"), hidden = false, surround = true)

    def altNode(chunks: List[Chunk[Doc]]): Chunk[Doc] =
      chunks match {
        case List(n) => n
        case ns      => ns.foldRight(Chunk.empty[Doc])(chunked(_.withSoftline(Doc.string("|")).withSoftline(_)))
          .map(_.parens)
      }

    def foldTree(tree: OptTree[Chunk[Doc]]): Chunk[Doc] =
      tree match {
        case Leaf(x) => x
        case MultNode(xs) => xs.foldRight(Chunk.empty[Doc])((x, y) => foldTree(x) <</>> y)
        case AltNode(xs) => altNode(xs.map(foldTree).filterNot(_.isEmpty))
      }

    foldTree(parser.treeMap(info => new (Opt ~> ({type λ[α]=Const[Chunk[Doc],α]})#λ) {
      def apply[AA](fa: Opt[AA]): Const[Chunk[Doc], AA] = Const(optDesc(pprefs, style, info, fa))
    }))
  }

  /** Generate a full help text for a parser. */
  def fullDesc[A](pprefs: ParserPrefs, parser: Parser[A]): Chunk[Doc] = {
    val style = OptDescStyle(sep = Doc.string(","), hidden = true, surround = false)

    tabulate(parser.mapPoly(info => new (Opt ~> ({type λ[α]=Const[Option[(Doc, Doc)],α]})#λ) {
      def apply[AA](fa: Opt[AA]): Const[Option[(Doc, Doc)], AA] = Const {
        val n = optDesc(pprefs, style, info, fa)
        val h = fa.props.help
        val hdef = Chunk(fa.props.showDefault.map(s => (Doc.string("default:") |+| Doc.string(s)).parens))
        (n.isEmpty || n.isEmpty).prevent[Option]((extract(n), extract(h <<+>> hdef))) //TODO check tabulation
      }
    }).flatten)
  }

  def errorHelp(chunk: Chunk[Doc]): ParserHelp =
    ParserHelp(chunk, empty, empty, empty, empty)

  def headerHelp(chunk: Chunk[Doc]): ParserHelp =
    ParserHelp(empty, chunk, empty, empty, empty)

  def usageHelp(chunk: Chunk[Doc]): ParserHelp =
    ParserHelp(empty, empty, chunk, empty, empty)

  def bodyHelp(chunk: Chunk[Doc]): ParserHelp =
    ParserHelp(empty, empty, empty, chunk, empty)

  def footerHelp(chunk: Chunk[Doc]): ParserHelp =
    ParserHelp(empty, empty, empty, empty, chunk)

  /** Generate the help text for a program. */
  def parserHelp[A](pprefs: ParserPrefs, parser: Parser[A]): ParserHelp = {
    def withTitle(title: String, chunk: Chunk[Doc]): Chunk[Doc] =
      chunk.map(Doc.string(title).withLine(_))

    bodyHelp(vsepChunks(List(withTitle("Available options:", fullDesc(pprefs, parser)),
      withTitle("Available commands:", cmdDesc(parser)))))
  }

  /** Generate option summary. */
  def parserUsage[A](pprefs: ParserPrefs, parser: Parser[A], progName: String): Doc =
    Doc.hsep(List(Doc.string("Usage:"), Doc.string(progName), extract(briefDesc(pprefs, parser)))) //TODO colt check align

}
