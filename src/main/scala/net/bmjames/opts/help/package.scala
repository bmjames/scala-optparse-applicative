package net.bmjames.opts

import net.bmjames.opts.types._
import net.bmjames.opts.common.showOption

import scalaz._
import scalaz.syntax.std.list._
import scalaz.syntax.functor._

package object help {

  import Pretty._
  import Chunk._

  /** Style for rendering an option. */
  final case class OptDescStyle(sep: Doc, hidden: Boolean, surround: Boolean)

  /** Generate description for a single option. */
  def optDesc[A](pprefs: ParserPrefs, style: OptDescStyle, info: OptHelpInfo, opt: Opt[A]): Chunk[Doc] = {
    val ns = opt.main.names
    val mv = Chunk.fromString(opt.props.metaVar)
    val descs = ns.sorted.map(string _ compose showOption)
    val desc = Chunk.fromList(descs.intersperse(style.sep)) <<+>> mv
    val vis = opt.props.visibility
    val showOpt = if (vis == Hidden) style.hidden else vis == Visible
    val suffix: Chunk[Doc] = if (info.multi) Chunk.fromString(pprefs.multiSuffix) else Chunk.zero

    def render(chunk: Chunk[Doc]): Chunk[Doc] =
      if (! showOpt) Chunk.zero
      else if (chunk.isEmpty || ! style.surround) chunk <> suffix
      else if (info.default) chunk.map(brackets) <> suffix
      else if (descs.drop(1).isEmpty) chunk <> suffix
      else chunk.map(parens) <> suffix

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
              yield (string(cmd), align(extractChunk(d)))
            )
          case _ => Chunk.zero
        })
    }))

  /** Generate a brief help text for a parser. */
  def briefDesc[A](pprefs: ParserPrefs, parser: Parser[A]): Chunk[Doc] = {
    val style = OptDescStyle(sep = "|", hidden = false, surround = true)

    def altNode(chunks: List[Chunk[Doc]]): Chunk[Doc] =
      chunks match {
        case List(n) => n
        case ns      => ns.foldRight(Chunk.zero[Doc])(chunked(_ </> char('|') </> _))
                          .map(parens)
      }

    def foldTree(tree: OptTree[Chunk[Doc]]): Chunk[Doc] =
      tree match {
        case Leaf(x) => x
        case MultNode(xs) => xs.foldRight(Chunk.zero[Doc])((x, y) => foldTree(x) <</>> y)
        case AltNode(xs) => altNode(xs.map(foldTree).filterNot(_.isEmpty))
      }

    foldTree(parser.treeMap(info => new (Opt ~> ({type λ[α]=Const[Chunk[Doc],α]})#λ) {
      def apply[A](fa: Opt[A]): Const[Chunk[Doc], A] = Const(optDesc(pprefs, style, info, fa))
    }))
  }

}
