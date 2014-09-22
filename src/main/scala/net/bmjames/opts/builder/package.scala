package net.bmjames.opts

import net.bmjames.opts.builder.internal.{HasName, Mod}
import net.bmjames.opts.types.OptShort
import scalaz.{EitherT, Applicative}

package object builder {

  def str[F[_]](s: String)(implicit F: Applicative[F]): F[String] =
    F.point(s)

  def disabled[F[_], A](e: String)(implicit F: Applicative[F]): EitherT[F, String, A] =
    EitherT.left[F, String, A](F.point(e))


  def short[F[_], A](c: Char)(implicit F: HasName[F]): Mod[F, A] =
    Mod.field(F.name[A](OptShort(c), _))

}
