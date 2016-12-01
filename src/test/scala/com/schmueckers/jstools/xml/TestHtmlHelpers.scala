package com.schmueckers.jstools.xml

import org.scalatest.FunSpec
import org.scalatest.Matchers
import com.schmueckers.jstools._
import com.schmueckers.jstools.HTMLHelpers._

class TestHtmlHelpers extends FunSpec with Matchers with XmlMatcher {
  describe("toHtml") {
    it("should leave HTML in fields untouched") {
      val t = Table("f" :: Nil, List(List(Some("<a href=\"abc\">ABC</a>"))))
      t.toHtml should beXml(<table>
                              <thead>
                                <tr>
                                  <th>f</th>
                                </tr>
                              </thead>
                              <tbody>
                                <tr>
                                  <td>
                                    <a href="abc">ABC</a>
                                  </td>
                                </tr>
                              </tbody>
                            </table>)
    }
  }

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
