package net.bmjames.opts.types

import net.bmjames.opts.common.{mapParser, treeMapParser}
import net.bmjames.opts.types.ParserM._
import scalaz.{NonEmptyList, ~>, Const, ApplicativePlus}
import scalaz.syntax.applicativePlus._

sealed trait Parser[A] {

  final def map[B](f: A => B): Parser[B] =
    this match {
      case NilP(fa)      => NilP(fa map f)
      case OptP(fa)      => OptP(fa map f)
      case MultP(p1, p2) => MultP(p1 map (_ andThen f), p2)
      case AltP(p1, p2)  => AltP(p1 map f, p2 map f)
      case BindP(p, k)   => BindP(p, k andThen (_ map f))
    }

  final def mapPoly[B](f: OptHelpInfo => (Opt ~> Const[B,?])): List[B] =
    mapParser[A, B](f, this)

  final def treeMap[B](g: OptHelpInfo => (Opt ~> Const[B,?])): OptTree[B] =
    treeMapParser[A, B](g, this)

  /** Alias for <+> */
  final def <|>(that: Parser[A]): Parser[A] = this <+> that
}

case class NilP[A](fa: Option[A]) extends Parser[A]

case class OptP[A](fa: Opt[A]) extends Parser[A]

case class MultP[A, B](p1: Parser[A => B], p2: Parser[A]) extends Parser[B]

case class AltP[A](p1: Parser[A], p2: Parser[A]) extends Parser[A]

case class BindP[A, B](p: Parser[A], f: A => Parser[B]) extends Parser[B]

object Parser extends ParserInstances with ParserFunctions

private[opts] trait ParserInstances {

  implicit val parserApplicativePlus: ApplicativePlus[Parser] =
    new ApplicativePlus[Parser] {
      override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
        fa.map(f)

      def ap[A, B](fa: => Parser[A])(f: => Parser[A => B]): Parser[B] =
        MultP(f, fa)

      def point[A](a: => A): Parser[A] = Parser.pure(a)

      def empty[A]: Parser[A] = NilP(None)

      def plus[A](a: Parser[A], b: => Parser[A]): Parser[A] = AltP(a, b)

      override def many[A](a: Parser[A]): Parser[List[A]] =
        Parser.many(a)

      override def some[A](a: Parser[A]): Parser[List[A]] =
        fromM(^(oneM(a), manyM(a))(_ :: _))

    }
}

private[opts] trait ParserFunctions {

  def pure[A](a: A): Parser[A] =
    NilP(Some(a))

  def many[A](p: Parser[A]): Parser[List[A]] =
    fromM(manyM(p))

  def some[A](p: Parser[A]): Parser[NonEmptyList[A]] =
    fromM(someM(p))

  def optional[A](p: Parser[A]): Parser[Option[A]] =
    p.map[Option[A]](Some(_)) <+> pure(None)
}
