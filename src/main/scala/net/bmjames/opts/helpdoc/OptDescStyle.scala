package net.bmjames.opts.helpdoc

import org.kiama.output.PrettyPrinter.Doc

/** Style for rendering an option. */
final case class OptDescStyle(sep: Doc, hidden: Boolean, surround: Boolean)
