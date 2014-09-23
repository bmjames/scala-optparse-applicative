package net.bmjames.opts.help

import org.kiama.output.PrettyPrinter.Doc
import org.kiama.output.{PrettyPrinter => PP}

import scalaz.Monoid

object Pretty {

  implicit val docMonoid: Monoid[Doc] =
    new Monoid[Doc] {
      def zero: Doc = PP.empty
      def append(f1: Doc, f2: => Doc): Doc = f1 <> f2
    }

}
