package com.schmueckers.tools.xml

import org.scalatest._
import matchers._
import scala.xml.Node
import scala.util.Try
import org.scalactic.Fail
import scala.util.Failure
import scala.util.Success

/**
 * A Custom Matcher implementation for scalatest that allows easy comparison of XML.
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
 */
trait XmlMatcher extends XmlCompare {

  class XmlMatcher(expected: Node) extends Matcher[Node] {

    /**
     * A somewhat easy to read matcher for XML
     * 
     * It's not perfect yet as it doesn't show only the broken part of the XML
     * in the display.
     */
    def apply(actual: Node) = {
      val p = new scala.xml.PrettyPrinter(40, 2)
      val pretty_e = p.format(scala.xml.Utility.trim(expected)).split("\n")
      val pretty_a = p.format(scala.xml.Utility.trim(actual)).split("\n")

      val z = pretty_e zipAll (pretty_a, "", "")

      def check(z: List[(String, String)]): Boolean =
        z match {
          case Nil    => true
          case h :: t => if (h._1 == h._2) check(t) else false
        }

      def combine(e_a: (String, String)): String = {
        val (e, a) = e_a

        val comp = if ( e == a ) "==" else "!="
        f"$e%-40s $comp%s $a%s"
      }
      
      MatchResult(
        check(z.toList),
        z.map(combine).mkString("\n"),
        z.map(combine).mkString("\n"))
    }
  }

  /**
   * The comparison method.
   * 
   * I couldn't figure out how to override the be method. Maybe somebody can
   * explain this to me.
   */
  def beXml(expected: Node) = new XmlMatcher(expected)
}

object XmlMatcher extends XmlMatcher

@deprecated("Didn't want to delete this")
trait XmlTestHelpers extends Matchers {
  protected def assertEqual(actual: Seq[Node], expected: Seq[Node]) {
    for {
      (a, e) <- actual.zipAll(expected, null, null)
    } assertEqual(a, e)
  }

  protected def assertEqual(actual: Node, expected: Node) {

    def recurse(actual: xml.Node, expected: xml.Node) {
      // depth-first checks, to get specific failures
      for ((actualChild, expectedChild) <- actual.child zip expected.child) {
        recurse(actualChild, expectedChild)
      }
      fail(s"Expected: ${expected} but received ${actual}")
    }
    recurse(scala.xml.Utility.trim(actual), scala.xml.Utility.trim(expected))
  }
}


@deprecated("Just didnt' want to delete this code")
trait XmlCompare {
  def compare(expected: Node, actual: Node): Option[Throwable] = {
    case class MatchException(expected: Node, found: Node) extends Exception("not needed")

    def recurse(actual: xml.Node, expected: xml.Node) {
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
