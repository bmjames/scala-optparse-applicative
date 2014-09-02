package net.bmjames.opts.types

sealed trait ParserResult[A]

case class Success[A](a: A) extends ParserResult[A]

case class Failure[A](failure: ParserFailure) extends ParserResult[A]

sealed trait ExitCode
case object ExitSuccess extends ExitCode
case class ExitFailure(code: Int) extends ExitCode

case class ParserFailure(run: String => (String, ExitCode))
