package net.bmjames.opts.types

import scalaz._, Scalaz._
import scala.annotation.tailrec

//Based on the paper http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

sealed abstract class Doc

//No text
case object EmptyDoc extends Doc
//A line break
case object LineDoc extends Doc
//Nest i spackes after any line break in the Doc.
case class NestDoc private (i: Int, d: Doc) extends Doc {
  override def toString: String = s"NestDoc($i, Doc(...))"
}
//Document representing the some text. text *must* not contain
//newlines.
case class TextDoc private (text: String) extends Doc
//Document which is the concatenation of 2 other documents.
case class ConsDoc private (d1: Doc, d2: Doc) extends Doc {
  override def toString: String = "ConsDoc(...)"
}
//A choice between 2 Docs. d1 and d2 must be the same doc when flattened and d1 must be longer
//Only exposed through group.
case class UnionDoc private (d1: Doc, d2: Doc) extends Doc {
  override def toString: String = "UnionDoc(...)"
}
//produce a document from the current "column" value.
case class ColumnDoc private (f: Int => Doc) extends Doc
//produce a document from the current nesting level.
case class NestingDoc private (f: Int => Doc) extends Doc

object Doc {
  import scala.language.implicitConversions
  //Implicit conversion to give syntax to `Doc`
  implicit def docToDocOps(d: Doc): DocOps = new DocOps(d)

  implicit val docMonoid: Monoid[Doc] = new Monoid[Doc] {
    def zero: Doc = empty
    def append(f1: Doc, f2: => Doc): Doc = ConsDoc(f1, f2)
  }

  //Primatives.
  //use `string` instead as it handles possible newlines.
  private def text(s: String): Doc = TextDoc(s)
  def nest(i: Int, d: Doc): Doc = NestDoc(i, d)
  def append(d1: Doc, d2: Doc) = ConsDoc(d1, d2)
  def column(f: Int => Doc): Doc = ColumnDoc(f)
  def nesting(f: Int => Doc): Doc = NestingDoc(f)

  //constants which are used frequently
  val space = text(" ")
  val empty: Doc = EmptyDoc
  val line: Doc = LineDoc
  val softLine:Doc = group(line)
  //Fold up the docs using f, empty if it's Nil.
  def foldDoc(docs: Seq[Doc])(f: (Doc, Doc) => Doc): Doc = docs match {
    case Nil => empty
    case docs => docs.reduceLeft(f)
  }
  //Separate the docs by a space.
  def hsep(docs: List[Doc]): Doc = foldDoc(docs.intersperse(space))(ConsDoc(_, _))

  //Convert a string literal into a Doc interpreting the `\n` chars an line breaks.
  def string(s: String): Doc = stringTramp(s).run

  private def stringTramp(s: String): Free.Trampoline[Doc] =
    if (s == "") {
      Trampoline.done(empty)
    } else if (s.head == '\n') {
      Trampoline.suspend(stringTramp(s.tail).map(line.append(_)))
    } else {
      val (xs, ys) = s.span(_ != '\n')
      Trampoline.suspend(stringTramp(ys).map(text(xs).append(_)))
    }

  def width(d: Doc, f: Int => Doc): Doc = column(k => d.append(column(k2 => f(k2 - k))))

  /**
   * Append spaces to d until it's requestedSpaces. If d is already wide enough, increase nesting
   * and add a line break.
   */
  def fillBreak(requestedWidth: Int, d: Doc): Doc = width(d, { w =>
    if (w > requestedWidth) {
      nest(requestedWidth, line)
    } else {
      //Insert the right amount of spaces
      spaces(requestedWidth - w)
    }
  })

  def hang(i: Int, d: Doc): Doc = align(nest(i, d))

  def indent(i:Int, d:Doc): Doc = hang(i, spaces(i).append(d))

  def align(d: Doc): Doc = column(current => nesting(indent => nest(current - indent, d)))

  def spaces(i: Int): Doc = text(" " * i)

  def enclose(l: Doc, m: Doc, r: Doc): Doc = append(append(l, m), r)

  def group(d: Doc): Doc = UnionDoc(flatten(d), d)

  def prettyRender(cols: Int, d: Doc): String = RenderDoc.prettyPrintToString(RenderDoc.fromDoc(cols, d))

  def flatten(d: Doc): Doc = flattenCore(d).run

  def flattenCore(d: Doc): Free.Trampoline[Doc] = d match {
    case EmptyDoc => Trampoline.done(EmptyDoc)
    case LineDoc => Trampoline.done(text(" "))
    case NestDoc(i, d) => Trampoline.suspend(flattenCore(d).map(nest(i, _)))
    case t: TextDoc => Trampoline.done(t)
    case ConsDoc(d1, d2) => for {
      flatD1 <- Trampoline.suspend(flattenCore(d1))
      flatD2 <- Trampoline.suspend(flattenCore(d2))
    } yield ConsDoc(flatD1, flatD2)
    case UnionDoc(d, _) => Trampoline.suspend(flattenCore(d))
    case ColumnDoc(f) => Trampoline.delay(ColumnDoc(f.andThen(flatten _)))
    case NestingDoc(f) => Trampoline.delay(NestingDoc(f.andThen(flatten _)))
  }

  final class DocOps(self: Doc) {
    def append(d2: Doc): Doc = Doc.append(self, d2)
    def withSpace(d: Doc): Doc = enclose(self, space, d)
    def withSoftline(d: Doc): Doc = enclose(self, softLine, d)
    def withLine(d: Doc): Doc = enclose(self, line, d)
    def brackets: Doc = enclose(text("["), self, text("]"))
    def parens: Doc = enclose(text("("), self, text(")"))
    def align: Doc = column(current => nesting(indent => nest(current - indent, self)))
  }
}

//ADT for a doc which can be rendered.
sealed abstract class RenderDoc

//Nothing
case object EmptyRenderDoc extends RenderDoc
//Render s followed by next
case class TextRenderDoc(s: String, next: RenderDoc) extends RenderDoc
//Render newline followed by i spaces and then d
case class LineRenderDoc(i: Int, d: RenderDoc) extends RenderDoc

object RenderDoc {
  //Convert a document to a RenderDoc with 0 spaces
  def fromDoc(width: Int, d: Doc): RenderDoc = findBest(width, 0, List((0, d)))

  //find the best layout using all the options in `l`. 
  def findBest(allowedWidth: Int, amountTaken: Int, l: List[(Int, Doc)]): RenderDoc =
    findBestCore(allowedWidth, amountTaken, l).run

  private def findBestCore(allowedWidth: Int, amountTaken: Int, l: List[(Int, Doc)]): Free.Trampoline[RenderDoc] = l match {
    case Nil => Trampoline.done(EmptyRenderDoc)
    case (i, EmptyDoc) :: tail => Trampoline.suspend(findBestCore(allowedWidth, amountTaken, tail))
    case (i, ConsDoc(d1, d2)) :: tail => Trampoline.suspend(findBestCore(allowedWidth, amountTaken, (i, d1) :: (i, d2) :: tail))
    case (i, NestDoc(j, d)) :: tail => Trampoline.suspend(findBestCore(allowedWidth, amountTaken, (i + j, d) :: tail))
    case (i, TextDoc(s)) :: tail => 
      Trampoline.suspend(findBestCore(allowedWidth, amountTaken + s.length, tail).map[RenderDoc](TextRenderDoc(s, _)))
    case (i, LineDoc) :: tail => Trampoline.suspend(findBestCore(allowedWidth, i, tail).map(LineRenderDoc(i, _)))
    case (i, UnionDoc(d1, d2)) :: tail =>
      for {
        first <- Trampoline.suspend(findBestCore(allowedWidth, amountTaken, (i, d1) :: tail))
        second <- Trampoline.suspend(findBestCore(allowedWidth, amountTaken, (i, d2) :: tail))
      } yield better(allowedWidth, amountTaken, first, second)
    case (i, ColumnDoc(f)) :: tail => Trampoline.suspend(findBestCore(allowedWidth, amountTaken, (i, f(amountTaken)) :: tail))
    case (i, NestingDoc(f)) :: tail => Trampoline.suspend(findBestCore(allowedWidth, amountTaken, (i, f(i)) :: tail))
  }

  def better(w: Int, k: Int, d1: RenderDoc, d2: RenderDoc): RenderDoc = if (fits(w - k, d1)) d1 else d2

  /**
   * Does the RenderDoc fit into the width.
   */
  @tailrec
  def fits(width: Int, doc: RenderDoc): Boolean = (width, doc) match {
    case (width, _) if (width < 0) => false
    case (width, EmptyRenderDoc) => true
    case (width, TextRenderDoc(s, next)) => fits(width - s.length, next)
    case (width, LineRenderDoc(_, _)) => true
  }

  /**
   * Render the document into a pretty string.
   */
  def prettyPrintToString(r: RenderDoc): String = {
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
