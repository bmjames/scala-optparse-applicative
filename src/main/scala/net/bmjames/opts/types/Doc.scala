package net.bmjames.opts.types

import scalaz._, Scalaz._
import scala.annotation.tailrec

sealed abstract class Doc

case object EmptyDoc extends Doc
case object LineDoc extends Doc
case class NestDoc(i: Int, d: Doc) extends Doc
case class TextDoc(text: String) extends Doc
case class ConsDoc(d1: Doc, d2: Doc) extends Doc
case class OrDoc(d1: Doc, d2: Doc) extends Doc
case class ColumnDoc(f: Int => Doc) extends Doc

object Doc {
  import scala.language.implicitConversions
  implicit def docToDocOps(d: Doc): DocOps = new DocOps(d)

  private[this] val SpaceDoc = text(" ")

  val empty: Doc = EmptyDoc
  val line: Doc = LineDoc
  def text(s: String): Doc = TextDoc(s)
  def nest(i: Int, d: Doc): Doc = NestDoc(i, d)
  def append(d1: Doc, d2: Doc) = ConsDoc(d1, d2)
  def column(f: Int => Doc): Doc = ColumnDoc(f)
  def foldDoc(s: Seq[Doc])(f: (Doc, Doc) => Doc): Doc = s match {
    case Nil => empty
    case s => s.reduceLeft(f)
  }
  def hsep(ds: Seq[Doc]): Doc = foldDoc(ds.toList.intersperse(SpaceDoc))(ConsDoc(_, _))

  def string(s: String): Doc =
    if (s == "") {
      empty
    } else if (s(0) == '\n') {
      line <> string(s.tail)
    } else {
      val (xs, ys) = s.span(_ != '\n')
      text(xs) <> string(ys)
    }

  def width(d: Doc, f: Int => Doc): Doc = column(k => d <> column(k2 => f(k2 - k)))

  def fillBreak(i: Int, d: Doc): Doc = width(d, { w =>
    if (w > i) {
      nest(i, line)
    } else {
      text(spaces(i - w))
    }
  })

  def spaces(i: Int): String = " " * i

  def enclose(l: Doc, m: Doc, r: Doc): Doc = append(append(l, m), r)

  def group(d: Doc): Doc = OrDoc(flatten(d), d)

  def flatten(d: Doc): Doc = d match {
    case EmptyDoc => EmptyDoc
    case LineDoc => text(" ")
    case NestDoc(i, d) => nest(i, flatten(d))
    case t: TextDoc => t
    case ConsDoc(d1, d2) => ConsDoc(flatten(d1), flatten(d2))
    case OrDoc(d, _) => flatten(d)
    case ColumnDoc(f) => ColumnDoc(f.andThen(flatten _))
  }

  def padtobreak(i: Int, d: Doc): Doc = fillBreak(i, d)

  def prettyRender(d: Doc, cols: Int): String = ???

  implicit val docMonoid: Monoid[Doc] = new Monoid[Doc] {
    def zero: Doc = empty
    def append(f1: Doc, f2: => Doc): Doc = ConsDoc(f1, f2)
  }

  final class DocOps(self: Doc) {
    def <>(d2: Doc): Doc = append(self, d2)
    def withSpace(d: Doc): Doc = enclose(self, SpaceDoc, d)
    def withSoftline(d: Doc): Doc = group(enclose(self, line, d))
    def withLine(d: Doc): Doc = enclose(self, line, d)
    def brackets: Doc = enclose(text("["), self, text("]"))
    def parens: Doc = enclose(text("("), self, text(")"))
  }
}

sealed abstract class RenderDoc

case object EmptyRenderDoc extends RenderDoc
case class TextRenderDoc(s: String, next: RenderDoc) extends RenderDoc
case class LineRenderDoc(i: Int, d: RenderDoc) extends RenderDoc

object RenderDoc {
  def fromDoc(width: Int, d: Doc): RenderDoc = findBest(width, 0, List((0, d)))

  def findBest(allowedWidth: Int, amountTaken: Int, l: List[(Int, Doc)]): RenderDoc = l match {
    case Nil => EmptyRenderDoc
    case (i, EmptyDoc) :: tail => findBest(allowedWidth, amountTaken, tail)
    case (i, ConsDoc(d1, d2)) :: tail => findBest(allowedWidth, amountTaken, (i, d1) :: (i, d2) :: tail)
    case (i, NestDoc(j, d)) :: tail => findBest(allowedWidth, amountTaken, (i + j, d) :: tail)
    case (i, TextDoc(s)) :: tail => TextRenderDoc(s, findBest(allowedWidth, amountTaken + s.length, tail))
    case (i, LineDoc) :: tail => LineRenderDoc(i, findBest(allowedWidth, amountTaken, tail))
    case (i, OrDoc(d1, d2)) :: tail => better(allowedWidth, amountTaken, findBest(allowedWidth, amountTaken, (i, d1) :: tail), findBest(allowedWidth, amountTaken, (i, d2) :: tail))
    case (i, ColumnDoc(f)) :: tail => findBest(allowedWidth, amountTaken, (i, f(i)) :: tail)
  }

  def better(w: Int, k: Int, d1: RenderDoc, d2: RenderDoc): RenderDoc = if (fits(w - k, d1)) d1 else d2

  @tailrec
  def fits(width: Int, doc: RenderDoc): Boolean = (width, doc) match {
    case (width, _) if (width < 0) => false
    case (width, EmptyRenderDoc) => true
    case (width, TextRenderDoc(s, next)) => fits(width - s.length, next)
    case (width, LineRenderDoc(_, _)) => true
  }

  def printToString(r: RenderDoc): String = {
    @tailrec
    def go(r: RenderDoc, sb: StringBuilder): StringBuilder = r match {
      case TextRenderDoc(s, next) =>
        sb.append(s)
        go(next, sb)
      case LineRenderDoc(i, d) =>
        sb.append('\n').append(" " * i)
        go(d, sb)
      case EmptyRenderDoc => sb
    }
    go(r, new StringBuilder()).toString
  }
}
