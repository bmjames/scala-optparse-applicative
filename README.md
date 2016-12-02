scala-optparse-applicative
==========================

A port of the [optparse-applicative] [1] library to the Scala programming language.

Most functionality has been ported, except completion.

This library depends on [Scalaz] [2] for functional data structures, type classes and combinators.

How to get it
-------------

Version 0.5 of scala-optparse-applicative is available for Scala 2.10, 2.11 and 2.12.

    resolvers += "bmjames Bintray Repo" at "https://dl.bintray.com/bmjames/maven"

    libraryDependencies += "net.bmjames" %% "scala-optparse-applicative" % "0.5"

License
-------
This library is distributed under a [BSD 3-Clause] [4] license (see `LICENSE`).

Simple example
--------------

This example follows the one from the [optparse-applicative] [1] docs.

```scala
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
      header("hello - a test for scala-optparse-applicative"))
    greet(execParser(args, "SampleMain", opts))
  }

}
```

When run with the `--help` option, it prints:

    hello - a test for scala-optparse-applicative
    
    Usage: SampleMain --hello TARGET [--quiet]
      Print a greeting for TARGET
    
    Available options:
      -h,--help                Show this help text
      --hello TARGET           Target for the greeting
      --quiet                  Whether to be quiet


Advanced examples
-----------------

Further examples can be found in `src/test/examples`.

To Do
-----

  * More tests

[1]: https://hackage.haskell.org/package/optparse-applicative
[2]: https://github.com/scalaz/scalaz
[4]: http://opensource.org/licenses/BSD-3-Clause
