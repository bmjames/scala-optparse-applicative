package net.bmjames.opts.test

import net.bmjames.opts.helpdoc.Chunk
import net.bmjames.opts.helpdoc.Pretty._
import net.bmjames.opts.internal._
import org.kiama.output.PrettyPrinter.{Doc, pretty}
import org.kiama.output.{PrettyPrinter => PP}

import scalaz.std.list._
import scalaz.std.string._
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

import Arbitrary._, Gen._

object ChunkSpec extends Properties("Chunk") {

  implicit def arbitraryChunk[A](implicit ev: Arbitrary[Option[A]]): Arbitrary[Chunk[A]] =
    Arbitrary(ev.arbitrary.map(Chunk(_)))

  def equalDocs(w: Int, d1: Doc, d2: Doc): Boolean =
    pretty(d1, w) == pretty(d2, w)

  property("fromList 1") = forAll { xs: List[String] => Chunk.fromList(xs).isEmpty == xs.isEmpty }

  property("fromList 2") = forAll { xs: List[String] => Chunk.fromList(xs) == xs.map(_.point[Chunk]).suml }

  property("extract 1") = forAll { s: String => Chunk.extract(s.point[Chunk]) == s }

  property("extract 2") = forAll { x: Chunk[String] => Chunk.extract(x.map(_.pure[Chunk])) == x }

  property("fromString 1") = forAll(posNum[Int], arbitrary[String]) { (w, s) =>
    equalDocs(w, Chunk.extract(Chunk.fromString(s)), PP.string(s))
  }

  property("fromString 2") = forAll { s: String => Chunk.fromString(s).isEmpty == s.isEmpty }

  property("paragraph") = forAll { s: String => Chunk.paragraph(s).isEmpty == words(s).isEmpty }
}
