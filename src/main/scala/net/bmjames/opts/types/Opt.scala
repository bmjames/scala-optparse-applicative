package net.bmjames.opts.types

import scalaz.Functor

/** A single option of a parser.
  */
trait Opt[A]

object Opt {

  implicit val optFunctor: Functor[Opt] = ???

}

sealed trait ArgPolicy
case object SkipOpts extends ArgPolicy
case object AllowOpts extends ArgPolicy
