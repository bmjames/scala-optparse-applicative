package net.bmjames.opts.common

import scalaz.Monoid

sealed trait MatchResult
case object NoMatch extends MatchResult
case class Match(s: Option[String]) extends MatchResult

object MatchResult {

  implicit def matchResultMonoid: Monoid[MatchResult] =
    new Monoid[MatchResult] {
      def zero: MatchResult = NoMatch
      def append(f1: MatchResult, f2: => MatchResult): MatchResult =
        f1 match {
          case Match(_) => f1
          case NoMatch  => f2
        }
    }
}
