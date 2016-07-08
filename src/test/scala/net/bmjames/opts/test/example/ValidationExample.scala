package net.bmjames.opts.test.example

import net.bmjames.opts._
import scalaz.{Applicative, Validation, ValidationNel, Success, Failure}
import scalaz.syntax.apply._

/** Demonstrates combining option parsing (which fails fast when it encounters errors)
  * with error-accumulating validation (using scalaz.Validation).
  */
object ValidationExample {

  case class UserData(username: String, email: String)

  type V[A] = ValidationNel[String, A]
  type ParserV[A] = Parser[V[A]]

  // Doesn't really validate emails, but gives us something to demonstrate
  def validEmail(email: String): V[String] =
    if (email.contains("@")) Validation.success(email)
    else Validation.failureNel("What kind of email address contains no '@' symbol?!")

  def validUsername(username: String): V[String] =
    if (username.length < 3) Validation.failureNel("That username is too short!")
    else if (username.length > 10) Validation.failureNel("That username is too looooong")
    else Validation.success(username)

  implicit val ParserVInstance: Applicative[ParserV] =
    Applicative[Parser] compose Applicative[V]

  val username: ParserV[String] = strOption(short('u'), long("username")).map(validUsername)
  val email: ParserV[String] = strOption(short('e'), long("email")).map(validEmail)

  val userData: ParserV[UserData] = ^(username, email)(UserData)

  def main(args: Array[String]): Unit = {
    val validatedUserData = execParser(args, "ValidationExample", info(userData))
    validatedUserData match {
      case Success(UserData(u, e)) =>
        println(s"Congratulations, $u <$e>, you are officially super-valid.")
      case Failure(errors) =>
        errors.foreach(System.err.println)
        System.exit(1)
    }
  }
}
