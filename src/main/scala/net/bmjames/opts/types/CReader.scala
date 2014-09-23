package net.bmjames.opts.types

import scalaz.{Kleisli, ReaderT}

object CReader {

  type CReader[F[_], A] = ReaderT[F, String, A]
  type OptCReader[A] = CReader[ReadM, A]
  type ArgCReader[A] = CReader[Option, A]

  def apply[F[_], A](f: String => F[A]): CReader[F, A] =
    Kleisli.kleisli[F, String, A](f)

}
