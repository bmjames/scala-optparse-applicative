package net.bmjames.opts.types

import scalaz.Functor
import scalaz.syntax.functor._

// As we don't have the completion functionality, this is serving as a useless wrapper
final case class CReader[A](reader: ReadM[A])

object CReader {

  implicit val cReaderFunctor: Functor[CReader] =
    new Functor[CReader] {
      def map[A, B](fa: CReader[A])(f: A => B): CReader[B] =
        CReader(fa.reader.map(f))
    }

}
