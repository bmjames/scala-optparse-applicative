package net.bmjames.opts.test.example

import net.bmjames.opts._

import scalaz.syntax.apply._

sealed trait Sample
case class Hello(targets: List[String]) extends Sample
case object Goodbye extends Sample

object Commands {

  val hello: Parser[Sample] = many(strArgument(metavar("TARGET..."))).map(Hello)

  val sample: Parser[Sample] =
    subparser(command("hello", info(hello, progDesc("Print greeting"))),
              command("goodbye", info(pure(Goodbye), progDesc("Say goodbye"))))

  def run: Sample => Unit = {
    case Hello(targets) => println(s"Hello, ${targets.mkString(", ")}!")
    case Goodbye => println("Goodbye.")
  }

  val opts: ParserInfo[Sample] = info(sample <*> helper)

  def main(args: Array[String]) {
    println(execParser(args, "SubparserExample", opts))
  }

}
