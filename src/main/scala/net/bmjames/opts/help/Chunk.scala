package net.bmjames.opts.help

import scalaz.{Applicative, Monoid, MonadPlus}
import scalaz.std.list._
import scalaz.syntax.monadPlus._
import scalaz.syntax.foldable._

import org.kiama.output.PrettyPrinter.{Doc, string}

/** The free monoid on a semigroup A */
case class Chunk[A](run: Option[A])

object Chunk {

  implicit val chunkMonadPlus: MonadPlus[Chunk] =
    new MonadPlus[Chunk] {
      def point[A](a: => A): Chunk[A] =
        Chunk(Some(a))

      def empty[A]: Chunk[A] =
        Chunk(None)

      def bind[A, B](fa: Chunk[A])(f: A => Chunk[B]): Chunk[B] =
        Chunk(fa.run.flatMap(f andThen (_.run)))

      def plus[A](a: Chunk[A], b: => Chunk[A]): Chunk[A] =
        ???
    }

  implicit def chunkMonoid[A](implicit A: Monoid[A]): Monoid[Chunk[A]] = ???

  implicit val docMonoid: Monoid[Doc] = ???

  /** Given a semigroup structure on A, return a monoid structure on Chunk[A] */
  def chunked[A](f: (A, A) => A): (Chunk[A], Chunk[A]) => Chunk[A] =
    {
      case (Chunk(None), y) => y
      case (x, Chunk(None)) => x
      case (Chunk(Some(x)), Chunk(Some(y))) => Chunk(Some(f(x, y)))
    }

  /** Concatenate a list into a Chunk. */
  def listToChunk[A: Monoid](as: List[A]): Chunk[A] =
    as match {
      case Nil => Monoid[Chunk[A]].zero
      case as  => as.foldMap().point[Chunk]
    }

  def words(s: String): List[String] =
    s.split("\\s+").toList

  def stringChunk(s: String): Chunk[Doc] =
    s match {
      case "" => Monoid[Chunk[Doc]].zero
      case s  => Applicative[Chunk].pure(string(s))
    }

  def paragraph(s: String): Chunk[Doc] =
    words(s).foldRight(Monoid[Chunk[Doc]].zero)((c, cs) => chunked[Doc](_ </> _)(stringChunk(c), cs))

}
