package net.bmjames.opts.help

import Chunk._
import scalaz.{Monoid, Show}
import scalaz.std.string._
import scalaz.syntax.semigroup._
import scalaz.syntax.show._

import org.kiama.output.PrettyPrinter.Doc
import org.kiama.output.{PrettyPrinter => PP}
import Pretty._

final case class ParserHelp(error:  Chunk[Doc],
                            header: Chunk[Doc],
                            usage:  Chunk[Doc],
                            body:   Chunk[Doc],
                            footer: Chunk[Doc])

object ParserHelp {

  implicit val parserHelpShow: Show[ParserHelp] =
    new Show[ParserHelp] {
      override def shows(f: ParserHelp): String = renderHelp(80, f).shows
    }

  implicit val parserHelpMonoid: Monoid[ParserHelp] =
    new Monoid[ParserHelp] {
      override def zero: ParserHelp =
        ParserHelp(Chunk.empty, Chunk.empty, Chunk.empty, Chunk.empty, Chunk.empty)
      override def append(f1: ParserHelp, f2: => ParserHelp): ParserHelp =
        ParserHelp(f1.error  |+| f2.error,
                   f1.header |+| f2.header,
                   f1.usage  |+| f2.usage,
                   f1.body   |+| f2.body,
                   f1.footer |+| f2.footer)
    }

  def helpText(help: ParserHelp): Doc =
    extractChunk(vsepChunks(List(help.error, help.header, help.usage, help.body, help.footer)))

  /** Convert a help text to a String */
  def renderHelp(cols: Int, help: ParserHelp): String =
    PP.pretty(helpText(help), cols)

}
