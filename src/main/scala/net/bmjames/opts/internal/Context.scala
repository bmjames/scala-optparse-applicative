package net.bmjames.opts.internal

import scalaz.Monoid
import net.bmjames.opts.types.ParserInfo

sealed trait Context
case class HasContext[A](names: List[String], p: ParserInfo[A]) extends Context
case object NullContext extends Context

object Context {

  def contextNames(c: Context): List[String] =
    c match {
      case HasContext(ns, _) => ns
      case NullContext       => Nil
    }

  implicit val contextMonoid: Monoid[Context] =
    new Monoid[Context] {

      def zero: Context = NullContext

      def append(f1: Context, f2: => Context): Context =
        f2 match {
          case HasContext(ns, i) => HasContext(contextNames(f1) ++ ns, i)
          case NullContext       => f1
        }
    }
}
