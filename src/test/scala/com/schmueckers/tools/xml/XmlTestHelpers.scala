package com.schmueckers.tools.xml

import org.scalatest.Matchers
import scala.xml.Node
import org.scalatest.matchers.Matcher
import scala.util.Try
import org.scalactic.Fail
import org.scalatest.matchers.MatchResult
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
