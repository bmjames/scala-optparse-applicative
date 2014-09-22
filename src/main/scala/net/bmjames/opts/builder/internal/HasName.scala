package net.bmjames.opts.builder.internal

import net.bmjames.opts.types.OptName


trait HasName[F[_]] {

  def name[A](n: OptName, fa: F[A]): F[A]

}
