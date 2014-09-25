scala-optparse-applicative
==========================

A port of the [optparse-applicative] [1] library to the Scala programming language.

Most functionality has been ported, except completion.

This library depends on [Scalaz] [2] for type classes and combinators, and [Kiama] [3] for pretty-printing.

Simple usage example
--------------------

    case class Sample(hello: String, quiet: Boolean)

    object SampleMain {

      val sample: Parser[Sample] =
        ^(
          strOption(long("hello"), metavar("TARGET"), help("Target for the greeting")),
          switch(long("quiet"), help("Whether to be quiet"))
        )(Sample.apply)

      def greet(s: Sample): Unit = s match {
        case Sample(h, false) => println("Hello, " ++ h)
        case _ =>
      }

      def main(args: Array[String]) {
        val opts = info(sample <*> helper,
          progDesc("Print a greeting for TARGET"),
          header("hello - a test for optparse-applicative"))
        greet(execParser(args, "SampleMain", opts))
      }

    }

[1]: https://hackage.haskell.org/package/optparse-applicative
[2]: https://github.com/scalaz/scalaz
[3]: https://code.google.com/p/kiama/
