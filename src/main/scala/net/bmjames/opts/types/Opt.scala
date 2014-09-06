package net.bmjames.opts.types

import scalaz.Functor

/** A single option of a parser.
  */
final case class Opt[A](main: OptReader[A], props: OptProperties)

/** Specification for an individual parser option.
  */
final case class OptProperties(visibility: OptVisibility)

object Opt {

  implicit val optFunctor: Functor[Opt] = ???

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
