package net.bmjames.opts.types

import scalaz.Monoid

sealed trait ParseError
case class ErrorMsg(msg: String) extends ParseError
case class InfoMsg(msg: String)  extends ParseError
case object ShowHelpText         extends ParseError
case object UnknownError         extends ParseError

object ParseError {

  implicit val parseErrorMonoid: Monoid[ParseError] =
    new Monoid[ParseError] {
      def zero: ParseError = UnknownError
      def append(f1: ParseError, f2: => ParseError): ParseError =
        f1 match {
          case UnknownError => f2
          case _            => f1
        }
    }

}
