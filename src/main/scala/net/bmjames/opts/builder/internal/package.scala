package net.bmjames.opts.builder

import net.bmjames.opts.common.liftOpt
import net.bmjames.opts.types._

import scalaz.syntax.plus._
import scalaz.syntax.std.option._

package object internal {

  def mkCommand[A](mod: Mod[CommandFields, A]): (List[String], String => Option[ParserInfo[A]]) = {
    val CommandFields(cmds) = mod.f(CommandFields(Nil))
    (cmds.map(_._1), cmds.toMap.lift)
  }

  def mkParser[A](prop: DefaultProp[A], g: OptProperties => OptProperties, reader: OptReader[A]): Parser[A] =
    liftOpt(mkOption(prop, g, reader)) <+> prop.fa.orEmpty[Parser]

  def mkOption[A](prop: DefaultProp[A], g: OptProperties => OptProperties, reader: OptReader[A]): Opt[A] =
    ???

}
