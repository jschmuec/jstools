package com.schmueckers.tools.xml

import org.scalatest._
import matchers._
import scala.xml.Node
import scala.util.Try
import org.scalactic.Fail
import scala.util.Failure
import scala.util.Success

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


trait XmlMatchers extends XmlCompare {

  class XmlMatcher(expected: Node) extends Matcher[Node] {

    def apply(found : Node ) = {
      val r = compare( expected, found ) 
      MatchResult(
        !r.isDefined,
        r.getOrElse("Not needed").toString,
        s"""${found} matched ${expected}"""
      )
    }
  }

  def beXml(expected : Node) = new XmlMatcher(expected)
}

object XmlMatchers extends XmlMatchers

trait XmlCompare {
  def compare(expected: Node, actual: Node) : Option[Throwable] = {
    case class MatchException(expected: Node, found: Node) extends Exception("not needed")

    def recurse(actual: xml.Node, expected: xml.Node) {
      // depth-first checks, to get specific failures
      for ((actualChild, expectedChild) <- actual.child zip expected.child) {
        recurse(actualChild, expectedChild)
      }
      throw new MatchException(expected, actual)
    }
    
    /* sorry just easier in procedure style */
    if ( expected == actual ) 
      return None
    if ( expected == null || actual == null ) 
      return Some(MatchException( expected, actual )) 
      
    Try {
      recurse(scala.xml.Utility.trim(actual), scala.xml.Utility.trim(expected))
    } match {
      case Success( s ) => None
      case Failure( f ) => Some(f)
    }
  }
  def compare( expected : Seq[Node], actual: Seq[Node] ) : Option[Throwable] = 
    expected.zipAll( actual, null, null).flatMap( x => compare( x._1, x._2 ) ).headOption
}
