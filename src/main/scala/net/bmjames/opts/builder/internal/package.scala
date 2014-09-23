package net.bmjames.opts.builder

import net.bmjames.opts.common.liftOpt
import net.bmjames.opts.helpdoc.Chunk
import net.bmjames.opts.types._

import scalaz.syntax.applicativePlus._
import scalaz.syntax.std.option._
import scalaz.std.option._

package object internal {

  def mkCommand[A](mod: Mod[CommandFields, A]): (List[String], String => Option[ParserInfo[A]]) = {
    val CommandFields(cmds) = mod.f(CommandFields(Nil))
    (cmds.map(_._1), cmds.toMap.lift)
  }

  def mkParser[A](prop: DefaultProp[A], g: OptProperties => OptProperties, reader: OptReader[A]): Parser[A] =
    liftOpt(mkOption(prop, g, reader)) <+> prop.default.orEmpty[Parser]

  def mkOption[A](prop: DefaultProp[A], g: OptProperties => OptProperties, reader: OptReader[A]): Opt[A] =
    Opt(reader, mkProps(prop, g))

  def mkProps[A](prop: DefaultProp[A], g: OptProperties => OptProperties): OptProperties =
    g(baseProps).copy(showDefault = prop.default <*> prop.sDef)

  val baseProps: OptProperties =
    OptProperties(metaVar = "", visibility = Visible, help = Chunk.empty, showDefault = None)

  /** Hide this option from the help text */
  def internal[F[_], A]: Mod[F, A] =
    Mod.option(_.copy(visibility = Internal))

}
