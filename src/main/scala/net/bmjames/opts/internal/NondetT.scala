package net.bmjames.opts.internal

import scalaz._
import scalaz.syntax.monad._

import ListT.listTMonadPlus

final case class NondetT[F[_], A](run: ListT[BoolState[F]#λ, A]) {

  import NondetT._

  def !(that: NondetT[F, A])(implicit F: Monad[F]): NondetT[F, A] = {
    val run2 = for {
      s  <- mState[F].get.liftM[ListT].filter(!_)
      a2 <- that.run
    } yield a2
    NondetT(ltmp[F].plus(run, run2))
  }

  def flatMap[B](f: A => NondetT[F, B])(implicit F: Monad[F]): NondetT[F, B] =
    NondetT(ltmp[F].bind(run)(f andThen (_.run)))

  def orElse(that: NondetT[F, A])(implicit F: Monad[F]): NondetT[F, A] =
    NondetT(ltmp[F].plus(run, that.run))
}

private[internal] trait BoolState[F[_]] {
  type λ[A] = StateT[F, Boolean, A]
}

object NondetT {

  def empty[F[_] : Monad, A]: NondetT[F, A] =
    NondetT(ltmp[F].empty)

  def pure[F[_] : Monad, A](a: => A): NondetT[F, A] =
    NondetT(ltmp[F].point(a))

  def cut[F[_]: Monad]: NondetT[F, Unit] =
    NondetT(mState[F].put(true).liftM[ListT])

  def disamb[F[_]: Monad, A](allowAmb: Boolean, xs: NondetT[F, A]): F[Option[A]] =
    xs.run
      .take(if (allowAmb) 1 else 2).run
      .eval(false)
      .map {
        case List(x) => Some(x)
        case _       => None
      }

  protected def ltmp[F[_]: Monad] = listTMonadPlus[BoolState[F]#λ]
  protected def mState[F[_]: Monad] = MonadState[StateT[F,Boolean,?], Boolean]

  implicit def nondetTMonadPlus[F[_] : Monad]: MonadPlus[NondetT[F,?]] =
    new MonadPlus[NondetT[F, ?]] {
      def bind[A, B](fa: NondetT[F, A])(f: A => NondetT[F, B]): NondetT[F, B] = fa.flatMap(f)

      def point[A](a: => A): NondetT[F, A] = NondetT.pure(a)

      def empty[A]: NondetT[F, A] = NondetT.empty

      def plus[A](a: NondetT[F, A], b: => NondetT[F, A]): NondetT[F, A] = a orElse b
    }

  implicit def nondetTTrans: MonadTrans[NondetT] =
    new MonadTrans[NondetT] {
      implicit def apply[G[_]: Monad]: Monad[NondetT[G,?]] =
        nondetTMonadPlus[G]

      def liftM[G[_]: Monad, A](a: G[A]): NondetT[G, A] =
        NondetT(StateT[G, Boolean, A](s => a.map(s -> _)).liftM[ListT])
    }
}
