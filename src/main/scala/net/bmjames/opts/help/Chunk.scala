package net.bmjames.opts.help

import scalaz.MonadPlus
import scalaz.syntax.plus._

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


}
