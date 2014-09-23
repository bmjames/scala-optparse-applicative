package net.bmjames.opts

import net.bmjames.opts.builder._
import net.bmjames.opts.types.{ShowHelpText, Parser}

import net.bmjames.opts.builder.internal.OptionFields

package object extra {

  /** A hidden "helper" option which always fails */
  def helper[A]: Parser[A => A] =
    abortOption(ShowHelpText, long[OptionFields, A => A]("help") <> short('h') <> help("Show this help text") <> hidden)

}
