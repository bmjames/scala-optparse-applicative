package net.bmjames.opts.types

import net.bmjames.opts.helpdoc.Chunk
import org.kiama.output.PrettyPrinter.Doc

import scalaz.Functor
import scalaz.syntax.functor._

/** A single option of a parser.
  */
final case class Opt[A](main: OptReader[A], props: OptProperties)

/** Specification for an individual parser option.
  */
final case class OptProperties(visibility: OptVisibility,
                               help: Chunk[Doc],
                               metaVar: String,
                               showDefault: Option[String])

object Opt {

  implicit val optFunctor: Functor[Opt] =
    new Functor[Opt] {
      def map[A, B](fa: Opt[A])(f: A => B): Opt[B] =
        fa.copy(main = fa.main.map(f))
    }

}

sealed trait ArgPolicy
case object SkipOpts extends ArgPolicy
case object AllowOpts extends ArgPolicy

final case class OptHelpInfo(multi: Boolean, default: Boolean)

sealed trait OptTree[A]
case class Leaf[A](a: A) extends OptTree[A]
case class MultNode[A](as: List[OptTree[A]]) extends OptTree[A]
case class AltNode[A](as: List[OptTree[A]]) extends OptTree[A]

sealed trait OptVisibility extends Ordered[OptVisibility] {
  private val toInt: OptVisibility => Int =
    Map(Internal -> 1, Hidden -> 2, Visible -> 3)
  def compare(that: OptVisibility): Int = toInt(this) - toInt(that)
}

case object Internal extends OptVisibility
case object Hidden extends OptVisibility
case object Visible extends OptVisibility
