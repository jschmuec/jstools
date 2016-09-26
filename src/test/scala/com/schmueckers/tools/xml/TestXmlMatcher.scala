package com.schmueckers.tools.xml

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Text
import com.schmueckers.tools._
import com.schmueckers.tools.xml.XmlMatchers._

class TestXmlMatcher extends FunSpec with Matchers {
  describe("The matcher") {
    it("shoudl work") {

      <a><b></b></a> should beXml(<a><b> </b></a>)
    }
    it("should fail") {
      <a></a> shouldNot beXml(<a>1</a>)
    }
  }
}