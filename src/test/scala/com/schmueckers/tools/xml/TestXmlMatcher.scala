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
      <a>   </a> should beXml(<a></a>)
    }
    it("should not match") {
      <a><b><c>CCC</c></b></a> shouldNot beXml(<a><b>1</b></a>)
    }
  }
}