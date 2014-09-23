package net.bmjames.opts.helpdoc

import scalaz.{Applicative, Monoid, MonadPlus}
import scalaz.std.list._
import scalaz.std.option._

import scalaz.syntax.std.option._
import scalaz.syntax.monadPlus._
import scalaz.syntax.foldable._

import org.kiama.output.PrettyPrinter.Doc
import org.kiama.output.{PrettyPrinter => PP}


/** The free monoid on a semigroup A */
final case class Chunk[A](run: Option[A]) {

  def isEmpty: Boolean = run.isEmpty

  def <>(that: => Chunk[A])(implicit A: Monoid[A]): Chunk[A] =
    Chunk.chunked[A]((f1, f2) => A.append(f1, f2))(this, that)
}

object Chunk {

  def empty[A]: Chunk[A] = Chunk(None)

  implicit val chunkMonadPlus: MonadPlus[Chunk] =
    new MonadPlus[Chunk] {
      def point[A](a: => A): Chunk[A] =
        Chunk(Some(a))

      def empty[A]: Chunk[A] =
        Chunk.empty

      def bind[A, B](fa: Chunk[A])(f: A => Chunk[B]): Chunk[B] =
        Chunk(fa.run.flatMap(f andThen (_.run)))

      def plus[A](a: Chunk[A], b: => Chunk[A]): Chunk[A] =
        Chunk(a.run <+> b.run)
    }

  implicit def chunkMonoid[A](implicit A: Monoid[A]): Monoid[Chunk[A]] =
    new Monoid[Chunk[A]] {
      def zero: Chunk[A] = Chunk.empty
      def append(f1: Chunk[A], f2: => Chunk[A]): Chunk[A] = f1 <> f2
    }

  /** Given a semigroup structure on A, return a monoid structure on Chunk[A] */
  def chunked[A](f: (A, A) => A): (Chunk[A], Chunk[A]) => Chunk[A] =
    {
      case (Chunk(None), y) => y
      case (x, Chunk(None)) => x
      case (Chunk(Some(x)), Chunk(Some(y))) => Chunk(Some(f(x, y)))
    }

  /** Concatenate a list into a Chunk. */
  def fromList[A: Monoid](as: List[A]): Chunk[A] =
    as match {
      case Nil => Monoid[Chunk[A]].zero
      case as  => as.foldMap().point[Chunk]
    }

  implicit class DocChunkSyntax(self: Chunk[Doc]) {

    /** Concatenate two Chunks with a space in between. */
    def <<+>>(that: Chunk[Doc]): Chunk[Doc] =
      chunked[Doc](_ <+> _)(self, that)

    /** Concatenate two Chunks with a softline in between */
    def <</>>(that: Chunk[Doc]): Chunk[Doc] =
      chunked[Doc](_ </> _)(self, that)
  }

  /** Concatenate Chunks vertically. */
  def vcatChunks(chunks: List[Chunk[Doc]]): Chunk[Doc] =
    chunks.foldRight(Chunk.empty[Doc])(chunked(_ <@> _))

  /** Concatenate Chunks vertically separated by empty lines. */
  def vsepChunks(chunks: List[Chunk[Doc]]): Chunk[Doc] =
    chunks.foldRight(Chunk.empty[Doc])(chunked((x, y) => x <@> PP.empty <@> y))

  def extractChunk[A : Monoid](chunk: Chunk[A]): A =
    chunk.run.orZero

  def words(s: String): List[String] =
    s.split("\\s+").toList

  def fromString(s: String): Chunk[Doc] =
    s match {
      case "" => Chunk.empty
      case s  => Applicative[Chunk].pure(PP.string(s))
    }

  def paragraph(s: String): Chunk[Doc] =
    words(s).foldRight(Chunk.empty[Doc])((c, cs) => chunked[Doc](_ </> _)(fromString(c), cs))

  def tabulate(table: List[(Doc, Doc)], size: Int = 24): Chunk[Doc] =
    table match {
      case Nil => Chunk.empty
      case xs  => Applicative[Chunk].pure(PP.vcat(
        for ((k, v) <- table) yield PP.indent(PP.padtobreak(size, k <+> v), 2)
      ))
    }
}
