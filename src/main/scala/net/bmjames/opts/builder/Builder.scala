package net.bmjames.opts.builder

import net.bmjames.opts.internal.min
import net.bmjames.opts.builder.internal._
import net.bmjames.opts.types._
import net.bmjames.opts.helpdoc.Chunk

import scalaz._
import scalaz.syntax.std.option._
import scalaz.syntax.semigroup._
import scalaz.syntax.monadPlus._
import scalaz.syntax.foldable._
import scalaz.std.option._
import scalaz.std.list._

import org.kiama.output.PrettyPrinter.Doc

private[opts] trait Builder {

  // Since Scalaz has no Read type class, there is no 'auto' function here.
  // Instead, I've implemented fromTryCatch, so you can use Scala's unsafe conversions such as 'toInt'

  /** String reader. */
  val readStr: ReadM[String] = ReadM.ask

  /** Int reader. */
  val readInt: ReadM[Int] = fromTryCatch(_.toInt)

  /** Char reader */
  val readChar: ReadM[Char] =
    ReadM.ask.flatMap { arg =>
      arg.toList match {
        case c :: Nil => c.point[ReadM]
        case _        => ReadM.error(s"cannot parse value `$arg'")
      }
    }

  /** Byte reader */
  val readByte: ReadM[Byte] = fromTryCatch(_.toByte)

  /** Short reader */
  val readShort: ReadM[Short] = fromTryCatch(_.toShort)

  /** Long reader */
  val readLong: ReadM[Long] = fromTryCatch(_.toLong)

  /** BigInt reader */
  val readBigInt: ReadM[BigInt] = fromTryCatch(BigInt.apply)

  /** Float reader */
  val readFloat: ReadM[Float] = fromTryCatch(_.toFloat)

  /** Double reader */
  val readDouble: ReadM[Double] = fromTryCatch(_.toDouble)

  /** BigDecimal reader */
  val readBigDecimal: ReadM[BigDecimal] = fromTryCatch(BigDecimal.apply)

  /** Turns an unsafe conversion function into a reader by catching non-fatal exceptions. */
  def fromTryCatch[A](f: String => A): ReadM[A] =
    ReadM.mkReadM { arg =>
      \/.fromTryCatchNonFatal(f(arg)).leftMap(_ => ErrorMsg(s"cannot parse value `$arg'"))
    }

  /** Null Option reader. All arguments will fail validation. */
  def disabled[A]: ReadM[A] =
    ReadM.error("disabled option")

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

  /** Specify the help text for an option. */
  def help[F[_], A](s: String): Mod[F, A] =
    Mod.option(_.copy(help = Chunk.paragraph(s)))

  /** Specify the help Doc. */
  def helpDoc[F[_], A](doc: Option[Doc]): Mod[F, A] =
    Mod.option(_.copy(help = Chunk(doc)))

  /** Convert a function in the Either monad to a reader. */
  def eitherReader[A](f: String => String \/ A): ReadM[A] =
    ReadM.ask.flatMap(arg => f(arg).fold(ReadM.error, _.point[ReadM]))

  /** Specify the error to display when no argument is provided to this option. */
  def noArgError[A](e: ParseError): Mod[OptionFields, A] =
    Mod.field(_.copy(noArgError = e))

  /** Specify a metavariable for the argument.
    *
    * Metavariables have no effect on the parser, and only serve to specify the symbolic name for
    * an argument to be displayed in the help text.
    */
  def metavar[F[_], A](v: String): Mod[F, A] =
    Mod.option(_.copy(metaVar = v))

  /** Hide this option from the brief description. */
  def hidden[F[_], A]: Mod[F, A] =
    Mod.option(p => p.copy(visibility = min(Hidden, p.visibility)))

  /** Add a command to a subparser option. */
  def command[A](cmd: String, info: ParserInfo[A]): Mod[CommandFields, A] =
    Mod.field(p => p.copy(commands = (cmd, info) :: p.commands))

  /** Builder for a command parser. The command modifier can be used to specify individual commands. */
  def subparser[A](mod: Mod[CommandFields, A]*): Parser[A] = {
    val m = mod.toList.suml
    val Mod(_, d, g) = metavar[CommandFields, A]("COMMAND") |+| m
    val reader = Function.tupled(CmdReader.apply[A] _)(mkCommand(m))
    mkParser(d, g, reader)
  }

  /** Builder for an argument parser. */
  def argument[A](p: ReadM[A], mod: Mod[ArgumentFields, A]*): Parser[A] = {
    val m = mod.toList.suml
    mkParser(m.prop, m.g, ArgReader(CReader(p)))
  }

  /** Builder for a String argument. */
  def strArgument(mod: Mod[ArgumentFields, String]*): Parser[String] = makeArgument(readStr, mod)

  /** Builder for a Int argument. */
  def intArgument(mod: Mod[ArgumentFields, Int]*): Parser[Int] = makeArgument(readInt, mod)

  /** Builder for a Char argument. */
  def charArgument(mod: Mod[ArgumentFields, Char]*): Parser[Char] = makeArgument(readChar, mod)

  /** Builder for a Byte argument. */
  def byteArgument(mod: Mod[ArgumentFields, Byte]*): Parser[Byte] = makeArgument(readByte, mod)

  /** Builder for a Short argument. */
  def shortArgument(mod: Mod[ArgumentFields, Short]*): Parser[Short] = makeArgument(readShort, mod)

  /** Builder for a Long argument. */
  def longArgument(mod: Mod[ArgumentFields, Long]*): Parser[Long] = makeArgument(readLong, mod)

  /** Builder for a BigInt argument. */
  def bigIntArgument(mod: Mod[ArgumentFields, BigInt]*): Parser[BigInt] = makeArgument(readBigInt, mod)

  /** Builder for a Float argument. */
  def floatArgument(mod: Mod[ArgumentFields, Float]*): Parser[Float] = makeArgument(readFloat, mod)

  /** Builder for a Double argument. */
  def doubleArgument(mod: Mod[ArgumentFields, Double]*): Parser[Double] = makeArgument(readDouble, mod)

  /** Builder for a BigDecimal argument. */
  def bigDecimalArgument(mod: Mod[ArgumentFields, BigDecimal]*): Parser[BigDecimal] = makeArgument(readBigDecimal, mod)

  private def makeArgument[A](readM: ReadM[A], mod: Seq[Mod[ArgumentFields, A]]): Parser[A] =
    argument(readM, mod.toList.suml)

  /** Builder for a flag parser. */
  def flag[A](defV: A, actV: A, mod: Mod[FlagFields, A]*): Parser[A] =
    flag_(actV, mod.toList.suml) <+> defV.pure[Parser]

  /** Builder for a flag parser without a default value. */
  def flag_[A](actV: A, mod: Mod[FlagFields, A]*): Parser[A] = {
    val m = mod.toList.suml
    val fields = m.f(FlagFields(Nil, actV))
    val reader = FlagReader(fields.names, fields.active)
    mkParser(m.prop, m.g, reader)
  }

  /** Builder for a boolean flag. */
  def switch(mod: Mod[FlagFields, Boolean]*): Parser[Boolean] =
    flag(false, true, mod.toList.suml)

  /** An option that always fails. */
  def abortOption[A](err: ParseError, mod: Mod[OptionFields, A => A]*): Parser[A => A] =
    option(ReadM.abort(err), noArgError[A => A](err) |+| value(identity) |+| metavar("") |+| mod.toList.suml)

  /** An option that always fails and displays a message. */
  def infoOption[A](s: String, mod: Mod[OptionFields, A => A]*): Parser[A => A] =
    abortOption(InfoMsg(s), mod.toList.suml)

  /** Builder for an option taking a String argument. */
  def strOption(mod: Mod[OptionFields, String]*): Parser[String] = makeOption(readStr, mod)

  /** Builder for an option taking a Int argument. */
  def intOption(mod: Mod[OptionFields, Int]*): Parser[Int] = makeOption(readInt, mod)

  /** Builder for an option taking a Char argument. */
  def charOption(mod: Mod[OptionFields, Char]*): Parser[Char] = makeOption(readChar, mod)

  /** Builder for an option taking a Byte argument. */
  def byteOption(mod: Mod[OptionFields, Byte]*): Parser[Byte] = makeOption(readByte, mod)

  /** Builder for an option taking a Short argument. */
  def shortOption(mod: Mod[OptionFields, Short]*): Parser[Short] = makeOption(readShort, mod)

  /** Builder for an option taking a Long argument. */
  def longOption(mod: Mod[OptionFields, Long]*): Parser[Long] = makeOption(readLong, mod)

  /** Builder for an option taking a BigInt argument. */
  def bigIntOption(mod: Mod[OptionFields, BigInt]*): Parser[BigInt] = makeOption(readBigInt, mod)

  /** Builder for an option taking a Float argument. */
  def floatOption(mod: Mod[OptionFields, Float]*): Parser[Float] = makeOption(readFloat, mod)

  /** Builder for an option taking a Double argument. */
  def doubleOption(mod: Mod[OptionFields, Double]*): Parser[Double] = makeOption(readDouble, mod)

  /** Builder for an option taking a BigDecimal argument. */
  def bigDecimalOption(mod: Mod[OptionFields, BigDecimal]*): Parser[BigDecimal] = makeOption(readBigDecimal, mod)

  private def makeOption[A](readM: ReadM[A], mod: Seq[Mod[OptionFields, A]]): Parser[A] =
    option(readM, mod.toList.suml)

  def option[A](r: ReadM[A], mod: Mod[OptionFields, A]*): Parser[A] = {
    val Mod(f, d, g) = metavar[OptionFields, A]("ARG") |+| mod.toList.suml
    val fields = f(OptionFields(Nil, ErrorMsg("")))
    val cReader = CReader(r)
    val reader = OptionReader(fields.names, cReader, fields.noArgError)
    mkParser(d, g, reader)
  }

  type InfoMod[A] = Endo[ParserInfo[A]]

  /** Specify a short program description. */
  def progDesc[A](desc: String): InfoMod[A] =
    Endo(_.copy(progDesc = Chunk.paragraph(desc)))

  def progDescDoc[A](doc: Option[Doc]): InfoMod[A] =
    Endo(_.copy(progDesc = Chunk(doc)))

  def failureCode[A](code: Int): InfoMod[A] =
    Endo(_.copy(failureCode = code))

  def noIntersperse[A]: InfoMod[A] =
    Endo(_.copy(intersperse = false))

  def header[A](header: String): InfoMod[A] =
    Endo(_.copy(header = Chunk.paragraph(header)))

  def headerDoc[A](doc: Option[Doc]): InfoMod[A] =
    Endo(_.copy(header = Chunk(doc)))

  import Chunk.empty

  def info[A](parser: Parser[A], mod: InfoMod[A]*): ParserInfo[A] = {
    val base = ParserInfo(parser = parser,
      fullDesc = true,
      progDesc = empty,
      header = empty,
      footer = empty,
      failureCode = 1,
      intersperse = true)
    mod.toList.suml.run(base)
  }

  type PrefsMod = Endo[ParserPrefs]

  def multiSuffix(suffix: String): PrefsMod =
    Endo(_.copy(multiSuffix = suffix))

  val disambiguate: PrefsMod =
    Endo(_.copy(disambiguate = true))

  val showHelpOnError: PrefsMod =
    Endo(_.copy(showHelpOnError = true))

  val noBacktrack: PrefsMod =
    Endo(_.copy(backtrack = false))

  def columns(cols: Int): PrefsMod =
    Endo(_.copy(columns = cols))

  def prefs(mod: PrefsMod*): ParserPrefs = {
    val base = ParserPrefs(multiSuffix = "",
      disambiguate = false,
      showHelpOnError = false,
      backtrack = true,
      columns = 80)
    mod.toList.suml.run(base)
  }

  /** Trivial option modifier. */
  def idm[M](implicit M: Monoid[M]): M =
    M.zero

}
