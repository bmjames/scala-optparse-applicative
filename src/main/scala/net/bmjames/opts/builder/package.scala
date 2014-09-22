package net.bmjames.opts

import net.bmjames.opts.builder.internal.{OptionFields, DefaultProp, HasName, Mod}
import net.bmjames.opts.types.{ParseError, ReadM, OptLong, OptShort}
import scalaz.{\/, Show, EitherT, Applicative}

package object builder {

  def str[F[_]](s: String)(implicit F: Applicative[F]): F[String] =
    F.point(s)

  def disabled[F[_], A](e: String)(implicit F: Applicative[F]): EitherT[F, String, A] =
    EitherT.left[F, String, A](F.point(e))


  /** Specify a short name for an option. */
  def short[F[_], A](c: Char)(implicit F: HasName[F]): Mod[F, A] =
    Mod.field(F.name[A](OptShort(c), _))

  /** Specify a long name for an option. */
  def long[F[_], A](s: String)(implicit F: HasName[F]): Mod[F, A] =
    Mod.field(F.name[A](OptLong(s), _))

  /** Specify a default value for an option. */
  def value[F[_], A](a: A): Mod[F, A] =
    Mod(identity, DefaultProp(Some(a), None), identity)

  /** Specify a function to show the default value for an option. */
  def showDefaultWith[F[_], A](f: A => String): Mod[F, A] =
    Mod(identity, DefaultProp(None, Some(f)), identity)

  /** Show the default value for this option using its Show instance. */
  def showDefault[F[_], A](implicit A: Show[A]): Mod[F, A] =
    showDefaultWith(a => A.show(a).toString)

  /** Convert a function in the Either monad to a reader. */
  def eitherReader[A](f: String => String \/ A): String => ReadM[A] =
    s => f(s).fold(ReadM.readerError, a => Applicative[ReadM].point(a))

  /** Specify the error to display when no argument is provided to this option. */
  def noArgError[A](e: ParseError): Mod[OptionFields, A] =
    Mod.field(_.copy(noArgError = e))
}
