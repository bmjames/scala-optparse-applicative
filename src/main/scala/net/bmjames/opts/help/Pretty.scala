package net.bmjames.opts.help

import org.kiama.output.PrettyPrinter

import scalaz.Monoid

object Pretty extends PrettyPrinter {

  implicit val docMonoid: Monoid[Doc] =
    new Monoid[Doc] {
      def zero: Doc = empty
      def append(f1: Doc, f2: => Doc): Doc = f1 <> f2
    }

}
