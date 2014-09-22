package net.bmjames.opts.builder.internal

import net.bmjames.opts.types.{ParseError, OptName}

case class OptionFields[A](names: List[OptName], noArgError: ParseError)

object OptionFields {

  implicit val optionFieldsHasName: HasName[OptionFields] =
    new HasName[OptionFields] {
      def name[A](n: OptName, fa: OptionFields[A]): OptionFields[A] =
        fa.copy(names = n :: fa.names)
    }
}

case class FlagFields[A](names: List[OptName], active: A)

object FlagFields {

  implicit val flagFieldsHasName: HasName[FlagFields] =
    new HasName[FlagFields] {
      def name[A](n: OptName, fa: FlagFields[A]): FlagFields[A] =
        fa.copy(names = n :: fa.names)
    }
}
