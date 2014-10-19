package net.bmjames

package object opts
  extends builder.Builder
  with common.Common
  with extra.Extra
  with helpdoc.Help
  with types.ParserFunctions {

  type Parser[A] = types.Parser[A]

}
