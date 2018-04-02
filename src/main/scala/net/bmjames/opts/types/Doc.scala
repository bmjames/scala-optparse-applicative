package net.bmjames.opts.types

import scalaz._, Scalaz._
import scala.collection.immutable.Queue
import Trampoline.{ suspend, done, delay }

//Doc implementation as laid out in https://www.cs.kent.ac.uk/pubs/2009/2847/content.pdf section 3.3
//The key is that it's *linear* bounded. Because of scala, we have to use an insane amount of Trampolines
//to pull it off. That idea taken from here: http://code.ouroborus.net/fp-syd/past/2013/2013-07-Sloane-InstallingTrampolines.pdf
//Additional combinators adapted from PPrint haskell lib.
//This is *insanely* ugly encoded in Scala, even the paper was hard to read. I'm very sorry.
object Doc {
  //Type aliases to try and give meaning to all the Ints and Functions
  type Indent = Int
  type Width = Int
  type Layout = String
  type Position = Int
  type Remaining = Int
  type Horizontal = Boolean
  type Out = Remaining => Free.Trampoline[Layout]
  type OutGroup = Horizontal => Out => Free.Trampoline[Out]
  type Dq = Queue[(Position, OutGroup)]
  type TreeCont = (Position, Dq) => Free.Trampoline[Out]
  type IW = (Doc.Indent, Width)
  type Cont = IW => TreeCont => Free.Trampoline[TreeCont]

  final val Empty: Doc = new Doc(_ => (c: TreeCont) => done(c))
  implicit val docMonoid: Monoid[Doc] = new Monoid[Doc] {
    def zero: Doc = Empty
    def append(f1: Doc, f2: => Doc): Doc = Doc.append(f1, f2)
  }

  //helpers for pruning, scanning, writing, etc
  def output(o: Out, r: Remaining, s: String): Free.Trampoline[String] = suspend(o(r).map(s ++ _))

  def scan(lengthOfText: Width, outGroup: OutGroup)(cont: TreeCont): Free.Trampoline[TreeCont] =
    delay(
      (p: Position, dq: Dq) =>
        dq.lastOption match {
          case Some((pos, group)) =>

            val obligation = (pos, (h: Horizontal) => (out1: Out) =>
              suspend(
                for {
                  out2 <- outGroup(h)(out1)
                  out3 <- group(h)(out2)
                } yield out3))
            //Add the obligation to the end and see if we can prune
            prune(cont)(p + lengthOfText, dq.init :+ obligation)
          case None =>
            //No choice but to print and move forward
            suspend(
              for {
                out1 <- cont(p + lengthOfText, Queue.empty)
                out2 <- outGroup(false)(out1)
              } yield out2)
        }
    )

  def prune(cont1: TreeCont): TreeCont =
    (p: Position, dq: Dq) =>
      done(
        (r: Remaining) =>
          dq.headOption match {
            case Some((s, grp)) =>
              if (p > s + r) {
                suspend(
                  for {
                    cont2 <- prune(cont1)(p, dq.tail)
                    out <- grp(false)(cont2)
                    layout <- out(r)
                  } yield layout)
              } else {
                suspend(
                  for {
                    out <- cont1(p, dq)
                    layout <- out(r)
                  } yield layout)
              }

            case None =>
              suspend(
                for {
                  out <- cont1(p, Queue.empty)
                  layout <- out(r)
                } yield layout)
          }
      )

  def leave(cont: TreeCont): TreeCont =
    (p: Position, dq: Dq) =>
      if (dq.isEmpty) {
        cont(p, Queue.empty)
      } else if (dq.length == 1) {
        val (s1, group1) = dq.last
        suspend(
          for {
            out1 <- cont(p, Queue.empty)
            out2 <- group1(true)(out1)
          } yield out2)
      } else {
        val (s1, group1) = dq.last
        val (s2, group2) = dq.init.last
        val obligation = (s2, (h: Horizontal) =>
          (out1: Out) => {
            val out3 =
              (r: Remaining) =>
                suspend(
                  for {
                    out2 <- group1(p <= s1 + r)(out1)
                    layout <- out2(r)
                  } yield layout)
            suspend(group2(h)(out3))
          })
        cont(p, dq.init.init :+ obligation)
      }

  //Primatives for providing an instance for Doc.
  def append(d1: Doc, d2: Doc): Doc = new Doc(iw => cont1 =>
    suspend(
      for {
        cont2 <- d2(iw)(cont1)
        c3 <- d1(iw)(cont2)
      } yield c3))

  def group(d: Doc): Doc = new Doc(iw => cont1 => {
    suspend(d(iw)(leave(cont1)).map { cont2 =>
      (pos: Position, dq: Dq) => {
        //obligation to write
        val obligation = (_: Horizontal) => (o: Out) => done(o)
        cont2(pos, dq :+ ((pos, obligation)))
      }
    })
  })

  /**
   * Output text on the line if horizontal is true, otherwise `\n` and indent.
   */
  private def line(text: String): Doc = new Doc({
    case (i, w) =>
      val textLength = text.length
      val outLine =
        (horizontal: Horizontal) => (o: Out) =>
          done(
            (r: Remaining) =>
              if (horizontal)
                output(o, r - textLength, text)
              else
                output(o, w - i, "\n" + " " * i)
          )
      scan(textLength, outLine)
  })
  def nest(j: Indent, d: Doc): Doc = new Doc({ case (i, w) => d((i + j, w)) })

  def text(s: String): Doc = new Doc({ iw =>
    val stringLength = s.length
    val outGroupFunc =
      (_: Horizontal) => (o: Out) =>
        done((r: Remaining) => output(o, r - stringLength, s))
    scan(stringLength, outGroupFunc)(_)
  })

  def column(f: Int => Doc): Doc = new Doc({
    case (indent, width) => cont =>
      done(
        (position: Position, dq: Dq) =>
          done(
            (remain: Remaining) =>
              for {
                cont1 <- f(width - remain)(indent, width)(cont)
                out <- cont1(position, dq)
                bp <- out(remain)
              } yield bp
          )
      )
  })

  def nesting(f: Int => Doc): Doc = new Doc({ case iw @ (i, _) => f(i)(iw) })

  //Derived combinators

  def hang(d: Doc, i: Indent): Doc = align(nest(i, d))

  def indent(i: Indent, d: Doc): Doc = hang(spaces(i).append(d), i)

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

  def string(s: String): Doc =
    if (s == "") {
      Empty
    } else if (s.head == '\n') {
      line.append(string(s.tail))
    } else {
      val (beforeNewLine, afterNewline) = s.span(_ != '\n')
      text(beforeNewLine).append(string(afterNewline))
    }

  def line: Doc = line(" ")

  def linebreak: Doc = line("")

  def width(d: Doc, f: Int => Doc): Doc = column(j => append(d, column(k => f(k - j))))

  def align(d: Doc): Doc = column(current => nesting(indent => nest(current - indent, d)))

  //Fold up the docs using f, empty if it's Nil.
  def foldDoc(docs: Seq[Doc])(f: (Doc, Doc) => Doc): Doc = docs match {
    case Nil => Empty
    case docs => docs.reduceLeft(f)
  }


  //Separate the docs by a space.
  def hsep(docs: List[Doc]): Doc = foldDoc(docs.intersperse(space))(append(_, _))

  def spaces(n: Int): Doc =
    if (n <= 0)
      Empty
    else
      text(" " * n)

  /**
   * Space if the resulting output fits on the line, otherwise it behaves like line.
   */
  def softLine: Doc = group(line)

  def space: Doc = char(' ')

  def char(c: Char): Doc = if (c == '\n') line else text(c.toString)

  def enclose(left: Doc, d: Doc, right: Doc): Doc = left.append(d).append(right)

  def prettyRender(w: Width, doc: Doc): String = {
    val end = (_: Position, _: Dq) => done((_: Remaining) => done(""))

    val trampResult = for {
      cont <- doc(0 -> w)(end)
      out <- cont(0, Queue.empty)
      result <- out(w)
    } yield result
    trampResult.run
  }
}

class Doc(step: Doc.Cont) { self =>
  import Doc._
  def apply(iw: IW): TreeCont => Free.Trampoline[TreeCont] = step(iw)

  def append(d2: Doc): Doc = Doc.append(self, d2)
  def withSpace(d: Doc): Doc = enclose(self, space, d)
  def withSoftline(d: Doc): Doc = enclose(self, softLine, d)
  def withLine(d: Doc): Doc = enclose(self, line, d)
  def brackets: Doc = enclose(text("["), self, text("]"))
  def parens: Doc = enclose(text("("), self, text(")"))
  def align: Doc = column(current => nesting(indent => nest(current - indent, self)))
  def pretty(width: Width): String = prettyRender(width, self)
}
