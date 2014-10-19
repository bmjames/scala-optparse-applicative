package net.bmjames.opts.helpdoc

import net.bmjames.opts.types._
import net.bmjames.opts.common.showOption

import scalaz._
import scalaz.std.option._
import scalaz.syntax.std.list._
import scalaz.syntax.std.boolean._
import scalaz.syntax.functor._

import org.kiama.output.PrettyPrinter.Doc
import org.kiama.output.{PrettyPrinter => PP}

private[opts] trait Help {

  import Pretty._
  import Chunk._

  /** Style for rendering an option. */
  final case class OptDescStyle(sep: Doc, hidden: Boolean, surround: Boolean)

  /** Generate description for a single option. */
  def optDesc[A](pprefs: ParserPrefs, style: OptDescStyle, info: OptHelpInfo, opt: Opt[A]): Chunk[Doc] = {
    val ns = opt.main.names
    val mv = Chunk.fromString(opt.props.metaVar)
    val descs = ns.sorted.map(PP.string _ compose showOption)
    val desc = Chunk.fromList(descs.intersperse(style.sep)) <<+>> mv
    val vis = opt.props.visibility
    val showOpt = if (vis == Hidden) style.hidden else vis == Visible
    val suffix: Chunk[Doc] = if (info.multi) Chunk.fromString(pprefs.multiSuffix) else Chunk.empty

    def render(chunk: Chunk[Doc]): Chunk[Doc] =
      if (! showOpt) Chunk.empty
      else if (chunk.isEmpty || ! style.surround) chunk <> suffix
      else if (info.default) chunk.map(PP.brackets) <> suffix
      else if (descs.drop(1).isEmpty) chunk <> suffix
      else chunk.map(PP.parens) <> suffix

    render(desc)
  }

  /** Generate descriptions for commands. */
  def cmdDesc[A](p: Parser[A]): Chunk[Doc] =
    Chunk.vcatChunks(p.mapPoly(_ => new (Opt ~> ({type λ[α]=Const[Chunk[Doc],α]})#λ) {
      def apply[A](fa: Opt[A]): Const[Chunk[Doc], A] =
        Const(fa.main match {
          case CmdReader(cmds, p) =>
            Chunk.tabulate(
              for (cmd <- cmds.reverse; d <- p(cmd).map(_.progDesc).toList)
              yield (PP.string(cmd), PP.align(extractChunk(d)))
            )
          case _ => Chunk.empty
        })
    }))

  /** Generate a brief help text for a parser. */
  def briefDesc[A](pprefs: ParserPrefs, parser: Parser[A]): Chunk[Doc] = {
    val style = OptDescStyle(sep = "|", hidden = false, surround = true)

    def altNode(chunks: List[Chunk[Doc]]): Chunk[Doc] =
      chunks match {
        case List(n) => n
        case ns      => ns.foldRight(Chunk.empty[Doc])(chunked(_ </> PP.char('|') </> _))
          .map(PP.parens)
      }

    def foldTree(tree: OptTree[Chunk[Doc]]): Chunk[Doc] =
      tree match {
        case Leaf(x) => x
        case MultNode(xs) => xs.foldRight(Chunk.empty[Doc])((x, y) => foldTree(x) <</>> y)
        case AltNode(xs) => altNode(xs.map(foldTree).filterNot(_.isEmpty))
      }

    foldTree(parser.treeMap(info => new (Opt ~> ({type λ[α]=Const[Chunk[Doc],α]})#λ) {
      def apply[A](fa: Opt[A]): Const[Chunk[Doc], A] = Const(optDesc(pprefs, style, info, fa))
    }))
  }

  /** Generate a full help text for a parser. */
  def fullDesc[A](pprefs: ParserPrefs, parser: Parser[A]): Chunk[Doc] = {
    val style = OptDescStyle(sep = ",", hidden = true, surround = false)

    tabulate(parser.mapPoly(info => new (Opt ~> ({type λ[α]=Const[Option[(Doc, Doc)],α]})#λ) {
      def apply[A](fa: Opt[A]): Const[Option[(Doc, Doc)], A] = Const {
        val n = optDesc(pprefs, style, info, fa)
        val h = fa.props.help
        val hdef = Chunk(fa.props.showDefault.map(s => PP.parens(PP.string("default:") <+> PP.string(s))))
        (n.isEmpty || n.isEmpty).prevent[Option]((extractChunk(n), PP.align(extractChunk(h <<+>> hdef))))
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
      chunk.map(PP.string(title) <@> _)

    bodyHelp(vsepChunks(List(withTitle("Available options:", fullDesc(pprefs, parser)),
      withTitle("Available commands:", cmdDesc(parser)))))
  }

  /** Generate option summary. */
  def parserUsage[A](pprefs: ParserPrefs, parser: Parser[A], progName: String): Doc =
    PP.hsep(List("Usage:", progName, PP.align(extractChunk(briefDesc(pprefs, parser)))))

}
