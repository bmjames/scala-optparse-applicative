package net.bmjames.opts.internal

import scalaz._
import scalaz.WriterT.{writerT, writerTHoist}
import scalaz.EitherT.eitherTHoist
import scalaz.syntax.monadPlus._
import scalaz.syntax.std.option._

import net.bmjames.opts.types.{ParseError, ParserPrefs, Parser, ParserInfo}

trait Completer

trait MonadP[F[_]] extends MonadPlus[F] {
  def setContext[A](s: Option[String], p: ParserInfo[A]): F[Unit]
  def setParser[A](s: Option[String], p: Parser[A]): F[Unit]
  def getPrefs: F[ParserPrefs]

  def missingArg[A](e: ParseError): F[A]
  def attempt[A](fa: F[A]): F[ParseError \/ A]
  def error[A](e: ParseError): F[A]
  def exit[A, B](p: Parser[B], a: Option[A]): F[A]
}

import P._

final case class P[A](run: P_[A])

object P {

  type P_[A] = EitherT[ContextWriter, ParseError, A]
  type ParserPrefsReader[A] = Reader[ParserPrefs, A]
  type ContextWriter[A] = WriterT[ParserPrefsReader, Context, A]

  def tell[F[_]: Applicative, W](w: W): WriterT[F, W, Unit] =
    writerT((w, ()).point[F])

  def hoistEither[F[_], A](fa: ParseError \/ A)(implicit F: MonadP[F]): F[A] =
    fa.fold(F.error, a => F.point(a))

  implicit val pMonadP: MonadP[P] =
    new MonadP[P] {
      def bind[A, B](fa: P[A])(f: A => P[B]): P[B] =
        P(fa.run.flatMap(f andThen (_.run)))

      def point[A](a: => A): P[A] = P(a.point[P_])

      def empty[A]: P[A] = P(PlusEmpty[P_].empty)

      def plus[A](a: P[A], b: => P[A]): P[A] = P(a.run <+> b.run)

      def setContext[A](name: Option[String], p: ParserInfo[A]): P[Unit] = {
        val set: ContextWriter[Unit] = tell(HasContext(name.toList, p))
        P(eitherTHoist[ParseError].liftM(set))
      }

      def setParser[A](s: Option[String], p: Parser[A]): P[Unit] =
        point(())

      def getPrefs: P[ParserPrefs] = {
        val ask: ContextWriter[ParserPrefs] =
          writerTHoist[Context].liftM(Kleisli.ask: ParserPrefsReader[ParserPrefs])
        P(eitherTHoist[ParseError].liftM(ask))
      }

      def missingArg[A](e: ParseError): P[A] =
        error(e)

      def attempt[A](fa: P[A]): P[ParseError \/ A] =
        P(eitherTHoist[ParseError].liftM(fa.run.run))

      def error[A](e: ParseError): P[A] =
        P(EitherT.left(e.point[ContextWriter]))

      def exit[A, B](p: Parser[B], a: Option[A]): P[A] =
        P(a.orEmpty[P_])
    }

}
