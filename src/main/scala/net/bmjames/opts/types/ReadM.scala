package net.bmjames.opts.types

import scalaz.{Kleisli, ReaderT, \/, MonadPlus}
import scalaz.syntax.applicativePlus._
import scalaz.syntax.either._

/** A newtype over the Either monad used by option readers.
  */
final case class ReadM[A](run: ReaderT[ParseError \/ ?, String, A])

object ReadM {

  def mkReadM[A](f: String => ParseError \/ A): ReadM[A] =
    ReadM(Kleisli[ParseError \/ ?, String, A](f))

  /** Return the value being read. */
  def ask: ReadM[String] =
    ReadM(Kleisli.ask[ParseError \/ ?, String])

  /** Abort option reader by exiting with a ParseError. */
  def abort[A](e: ParseError): ReadM[A] =
    mkReadM(_ => e.left)

  /** Abort option reader by exiting with an error message. */
  def error[A](e: String): ReadM[A] =
    abort(ErrorMsg(e))

  implicit val readMMonadPlus: MonadPlus[ReadM] =
    new MonadPlus[ReadM] {
      def bind[A, B](fa: ReadM[A])(f: A => ReadM[B]): ReadM[B] =
        ReadM(fa.run.flatMap(a => f(a).run))

      def point[A](a: => A): ReadM[A] =
        mkReadM(_ => a.right)

      def empty[A]: ReadM[A] =
        mkReadM(_ => UnknownError.left)

      def plus[A](a: ReadM[A], b: => ReadM[A]): ReadM[A] =
        mkReadM(s => a.run.run(s) <+> b.run.run(s))
        //a.run.fold(_ => b, a => a.point[ReadM])
    }
}
