package net.bmjames.opts.types

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

import Arbitrary._, Gen._

object DocSpec extends Properties("Doc") {
  val stringDocGen = arbitrary[String].map(Doc.string(_))
  val lineOrEmptyDocGen = Gen.oneOf(Doc.line, Doc.Empty)
  val appendDocGen = Gen.listOfN(10, Gen.frequency((3, stringDocGen), (1, lineOrEmptyDocGen))).map{
    _.reduce(Doc.append(_,_))
  }
  implicit val arbDoc = Arbitrary(appendDocGen)
  def equalDocs(w: Int, d1: Doc, d2: Doc): Boolean =
    Doc.prettyRender(w, d1) == Doc.prettyRender(w, d2)

  property("text append is same as concat of strings") = forAll(posNum[Int], arbitrary[String], arbitrary[String]){ (w, s1, s2) =>
   equalDocs(w, Doc.string(s1 ++ s2), Doc.append(Doc.string(s1), Doc.string(s2)))
  }

  property("nesting law") = forAll(posNum[Int], posNum[Int], posNum[Int], arbitrary[Doc]){ (w, w2, w3, doc) =>    
   val List(nest1, nest2, width) = List(w,w2,w3).sorted
   equalDocs(w, Doc.nest(nest1 + nest2, doc), Doc.nest(nest1, Doc.nest(nest2, doc)))
  }

  property("zero nesting is id") = forAll(posNum[Int], arbitrary[Doc]){ (w, doc) =>
    equalDocs(w, Doc.nest(0, doc), doc)
  }

  property("nesting distributes") = forAll(posNum[Int], posNum[Int], arbitrary[Doc], arbitrary[Doc]){ (w, w2, doc, doc2) =>
    val List(nesting, width) = List(w,w2).sorted
    equalDocs(width, Doc.nest(nesting, Doc.append(doc, doc2)), Doc.nest(nesting, doc).append(Doc.nest(nesting, doc2)))
  }

  property("nesting single line is noop") = forAll(posNum[Int], posNum[Int], arbitrary[String]){ (w, w2, s) =>
    val List(nesting, width) = List(w,w2).sorted
    val noNewlines = s.filter(_ != '\n')
    equalDocs(width, Doc.nest(nesting, Doc.string(noNewlines)), Doc.string(noNewlines))
  }
}
