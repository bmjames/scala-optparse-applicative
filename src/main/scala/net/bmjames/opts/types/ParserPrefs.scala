package net.bmjames.opts.types

/**
  * @param multiSuffix metavar suffix for multiple options
  * @param disambiguate automatically disambiguate abbreviations
  * @param showHelpOnError show help text on parse errors
  * @param backtrack backtrack to parent parser when a subcommand fails
  * @param columns format the help page
  */
final case class ParserPrefs(multiSuffix: String,
                             disambiguate: Boolean = false,
                             showHelpOnError: Boolean = false,
                             backtrack: Boolean = false,
                             columns: Int = 80)
