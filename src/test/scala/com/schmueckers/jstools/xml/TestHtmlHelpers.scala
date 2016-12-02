package com.schmueckers.jstools.xml

import org.scalatest.FunSpec
import org.scalatest.Matchers
import com.schmueckers.jstools._
import com.schmueckers.jstools.HTMLHelpers._

class TestHtmlHelpers extends FunSpec with Matchers with com.schmueckers.jstools.xml.XmlMatcher {
  describe("html_escape_amp") {
    it("should replace & with &amp;") {
      escape_amp("a&b") should be("a&amp;b")
    }
    it("should not replace &auml;") {
      val s = "a&auml;b"
      escape_amp(s) should be(s)
    }
  }
  describe("ElemExtensions") {
    it("should add an attribute correctly") {
      val e = <a/>
      e.add_attribute("i", "1") should beXml(<a i="1"/>)
    }
    it("should allow to override an attribute~") {
      val e = <a i="1"/>
      e.add_attribute("i", "2") should beXml(<a i="2"/>)
    }
  }
}
