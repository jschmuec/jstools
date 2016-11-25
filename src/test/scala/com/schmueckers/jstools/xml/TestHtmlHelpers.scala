package com.schmueckers.jstools.xml

import org.scalatest.FunSpec
import org.scalatest.Matchers
import com.schmueckers.jstools._
import com.schmueckers.jstools.HTMLHelpers._

class TestHtmlHelpers extends FunSpec with Matchers with XmlMatcher {
  describe("toHtml") {
    it("should leave HTML in fields untouched") {
      val t = new SeqTable("f" :: Nil, List(List(Some("<a href=\"abc\">ABC</a>"))))
      t.toHtml should beXml(<table>
                              <thead>
                                <tr>
                                  <th>f</th>
                                </tr>
                              </thead>
                              <tbody>
                                <tr>
                                  <td>                                    <a href="abc">ABC</a>
                                  </td>
                                </tr>
                              </tbody>
                            </table>)
    }
  }

  describe("html_escape_amp") {
    it("should replace & with &amp;") {
      escape_amp("a&b") should be ("a&amp;b")
    }
    it("should not replace &auml;") {
      val s = "a&auml;b"
      escape_amp( s) should be (s)
    }
  }
}
