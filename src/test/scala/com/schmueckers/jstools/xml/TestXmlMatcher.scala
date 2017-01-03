package com.schmueckers.jstools.xml

import org.scalatest.{FunSpec, Inside, Matchers}

class TestXmlMatcher extends FunSpec with Matchers with XmlMatcher with Inside {
  describe("The matcher") {
    it("shoudl work") {

      <a><b></b></a> should beXml(<a><b> </b></a>)
      <a>   </a> should beXml(<a></a>)
    }
    it("should not match") {
      <a><b><c>CCC</c></b></a> shouldNot beXml(<a><b>1</b></a>)
    }
  }
  describe("An XmlMatcher") {
    it("Should be able to compare Xml like diff") {
      val x1 = <a><c/></a>
      val x2 = <a><b></b><c/></a>
      val padding = 10

      val rawResult: List[(String, String)] = List(("<a>", "<a>"), ("", "<b/>"), ("<c/>", "<c/>"), ("</a>", "</a>"))
      val compResult = rawResult.map(
        ae => {
          val (a, e) = ae
          s"${a.padTo(padding, ' ')} ${if (a == e) "==" else "!="} $e"

        }).mkString("\n")

      val m = new XmlMatcher(x1, padding)
      val r = m(x2)
      r shouldNot be('matches)

      r.failureMessage should be(compResult)

    }
    it("should handle unparsed correctly") {
      val x1 =
        <td><a href="abc"/></td>

      val x2 =
        <td>
          {scala.xml.Unparsed("<a href=\"abc\"/>")}
        </td>
      
            
      x1 should beXml(x2)
    }
  }
}
