package com.schmueckers.jstools.xml

import org.scalatest._
import org.scalatest.matchers._
import scala.xml.Node
import scala.util.Try
import scala.util.Failure
import scala.util.Success

/** A Custom Matcher implementation for scalatest that allows easy comparison of XML.
  *
  * Usage:
  *
  * {{{
  * class TestSometing with XmlMatcher {
  * 	<a></a> should beXml(<a></a>)
  * }
  * }}}
  *
  * or
  * {{{
  * import com.schmueckers.tools.xml.XmlMatcher._
  *
  * class TestSomething {
  * 	<a></a> should beXml(<a></a>)
  * }
  * }}}
  *
  * The following test creates the outputs
  * {{{
  * class TestXmlMatcher extends FunSpec with Matchers with XmlMatcher with Inside {
  * describe("The matcher") {
  * it("should fail") {
  * <a><b><c>CCC</c></b></a> should beXml(<a><b>1</b></a>)
  * }
  * }
  * }}}
  *
  * <cdata>
  * [Info] The matcher
  * [info] - shoudl work
  * [info] - should not match *** FAILED ***
  * [info]   <a>                                      == <a>
  * [info]   <b>1</b>                                 !=
  * [info]                                            != <b>
  * [info]                                            != <c>CCC</c>
  * [info]                                            != </b>
  * [info]   </a>                                     == </a> (TestXmlMatcher.scala:20)
  * </cdata>
  *
  */
trait XmlMatcher extends XmlCompare {

  /** Matcher for XML that outputs formatted comparison
    *
    * @param padding defines how wide the printed XML will be formatted to
    *
    * After looking at libraries out there which did much smarter stuff I
    * decided to implement a very simple diff algo on the XML in formatted form.
    * The algo has O(N*N) if I'm not wrong, so don't run it on massive data
    * sets.
    *
    */
  class XmlMatcher(expected: Node, padding: Int = 40) extends Matcher[Node] {

    def apply(actual: Node) = {
      /** Normalize the XML to remove Unparsed elements
        */
      def normalize(n: Node) =
        scala.xml.XML.loadString(n.buildString(false))

      val p = new scala.xml.PrettyPrinter(padding, 2)
      val pretty_e = p.format(scala.xml.Utility.trim(normalize(expected))).split("\n")
      val pretty_a = p.format(scala.xml.Utility.trim(normalize(actual))).split("\n")

      def diff(a: List[String], e: List[String]): List[(String, String)] = a match {
        case Nil => a.zipAll(e, "", "")
        case h :: t => e.indexOf(h) match {
          case -1 => (h, "") :: diff(t, e)
          case 0  => (h, e(0)) :: diff(t, e.drop(1))
          case i  => e.take(i).map(a => ("", a)) ++ ((h, e(i)) :: diff(t, e.drop(i + 1)))
        }
      }

      val z = diff(pretty_e.toList.map(_.trim), pretty_a.toList.map(_.trim))

      def combine(e_a: (String, String)): String = {
        val (e, a) = e_a

        val comp = if (e == a) "==" else "!="
        s"${e.padTo(padding, ' ')} ${comp} $a"
      }

      MatchResult(
        z.forall(ab => ab._1 == ab._2),
        z.map(combine).mkString("\n"),
        z.map(combine).mkString("\n"))
    }
  }

  /** The comparison method.
    *
    * I couldn't figure out how to override the be method. Maybe somebody can
    * explain this to me.
    */
  def beXml(expected: Node) = new XmlMatcher(expected)
}

object XmlMatcher extends XmlMatcher

@deprecated("Didn't want to delete this", "0.5")
trait XmlTestHelpers extends Matchers {
  protected def assertEqual(actual: Seq[Node], expected: Seq[Node]): Unit = {
    for {
      (a, e) <- actual.zipAll(expected, null, null)
    } assertEqual(a, e)
  }

  protected def assertEqual(actual: Node, expected: Node): Unit = {

    def recurse(actual: xml.Node, expected: xml.Node): Unit = {
      // depth-first checks, to get specific failures
      for ((actualChild, expectedChild) <- actual.child zip expected.child) {
        recurse(actualChild, expectedChild)
      }
      fail(s"Expected: ${expected} but received ${actual}")
    }
    recurse(scala.xml.Utility.trim(actual), scala.xml.Utility.trim(expected))
  }
}

@deprecated("Just didnt' want to delete this code", "0.5")
trait XmlCompare {
  def compare(expected: Node, actual: Node): Option[Throwable] = {
    case class MatchException(expected: Node, found: Node) extends Exception("not needed")

    def recurse(actual: xml.Node, expected: xml.Node) : Unit = {
      // depth-first checks, to get specific failures
      for ((actualChild, expectedChild) <- actual.child zip expected.child) {
        recurse(actualChild, expectedChild)
      }
      if (expected != actual)
        throw new MatchException(expected, actual)
    }

    /* sorry just easier in procedure style */
    if (expected == actual)
      return None
    if (expected == null || actual == null)
      return Some(MatchException(expected, actual))

    Try {
      recurse(scala.xml.Utility.trim(actual), scala.xml.Utility.trim(expected))
    } match {
      case Success(s) => None
      case Failure(f) => Some(f)
    }
  }
  def compare(expected: Seq[Node], actual: Seq[Node]): Option[Throwable] =
    expected.zipAll(actual, null, null).flatMap(x => compare(x._1, x._2)).headOption
}
