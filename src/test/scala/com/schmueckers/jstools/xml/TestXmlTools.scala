package com.schmueckers.jstools.xml

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Text
import scala.xml.Unparsed
import com.schmueckers.jstools.xml.XmlMatcher
import com.schmueckers.jstools.xml.XmlCompare

class TestXmlTools extends FunSpec with GivenWhenThen with Matchers with XmlCompare with XmlMatcher {

  describe("NewTransformer") {
    val doc = <a><b><a>1</a></b></a>
    val id = (s: Int, n: Node) => (s, Seq(n))

    it("Should return an unchanged Seq[node] when use with the identity function") {

      Given("A simple Seq of xml node")
      val n = <a>a</a>
      val r = n.transform(id)(0)
      r should be((0, Seq(n)))
    }
    it("should return a single node with text in it unchanged") {
      Given("A simple xml node")
      val simple = <a>1</a>

      When("Appling an identity transformation")
      val result = simple.transform(id)(0)

      Then("The result should be a Seq of the same node")
      result should be((0, Seq(simple)))
    }
    it("should map a text correctly") {
      val simple = <a>1</a>
      def t(n: Node) = Seq(n match {
        case Text("1") => Text("Found")
        case other     => other
      })
      simple.tmap(t _) should be(Seq(<a>Found</a>))
    }
    it("should call the transform method once for each node and propagate state") {

      def count(state: Int, n: Node): (Int, Seq[Node]) = {
        (state + 1, List(<b/>))
      }
      val ret = doc.transform(count _)(0)

      ret._1 should be(4) // don't forget the {{{Text}}}
    }
    it("should apply transform to matching nodes") {
      def as_to_bs(n: Node) = n match {
        case Elem(null, "a", attribs, scope, children @ _*) =>
          Seq(Elem(null, "A", attribs, scope, children: _*))
        case other => Seq(other)
      }
      val ret = doc.tmap(as_to_bs _)
      ret should be(Seq(<A><b><A>1</A></b></A>))
    }
    it("should Unparsed elements untouched") {
      def dropBs( n : Node ) = n match {
        case Elem( null, "b", attribs, scope, children @ _ * ) => List()
        case default => default
      }
      val x = <a>{ new Unparsed("<b>hello world</b>") }<b>drop this</b></a>
      x.tmap( dropBs ).head should beXml( <a><b>hello world</b></a> )
    }
  }
}