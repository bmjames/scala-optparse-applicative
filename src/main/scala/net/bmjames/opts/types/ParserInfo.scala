package net.bmjames.opts.types

import net.bmjames.opts.helpdoc.Chunk

import scalaz.Functor
import scalaz.syntax.functor._

import org.kiama.output.PrettyPrinter.Doc

/** A full description for a runnable Parser for a program.
  *
  * @param parser the option parser for the program
  * @param fullDesc whether the help text should contain full documentation
  * @param failureCode exit code for a parser failure
  * @tparam A
 */
final case class ParserInfo[A](parser: Parser[A],
                              fullDesc: Boolean,
                              progDesc: Chunk[Doc],
                              header: Chunk[Doc],
                              footer: Chunk[Doc],
                              failureCode: Int,
                              intersperse: Boolean)

object ParserInfo {

  implicit val parserInfoFunctor: Functor[ParserInfo] =
    new Functor[ParserInfo] {
      def map[A, B](fa: ParserInfo[A])(f: A => B): ParserInfo[B] =
        fa.copy(parser = fa.parser.map(f))
    }
}
