package com.schmueckers.jstools.xml

import org.scalatest.FunSpec
import org.scalatest.Matchers
import scala.xml.NodeSeq

class TestNodeEnhancers extends FunSpec with Matchers {
  describe("NodeEnhancer.\\@ should") {
    it("find all nodes with a given attribute and value") {
      val x = <a>
                <b key="1">test</b>
                <b>test</b>
                <c key="1">ctest</c>
              </a>
      val nodes = x \\@ ("key", "1")
      nodes.size should be(2)
    }
  }
  val testXml = <a>
                  <n id="1">First</n>
                  <n id="2">Nothing</n>
                  <n id="1">Second</n>
                </a>
  describe("NodeExtensions.where <id> is <value> should") {
    it("should return empty NodeSeq if no elements match the criteria") {
      testXml where "id" is "3" should be(NodeSeq.Empty)
    }
    it("should find a single match") {
      testXml where "id" is "2" should be(NodeSeq.fromSeq(Seq(<n id="2">Nothing</n>)))
    }
    it("should not find anything if searching for the wrong key") {
      testXml where "key" is "1" should be(NodeSeq.Empty)
    }
    it("should find all the elements that actually have matching attributes") {
      testXml where "id" is "1" should be(NodeSeq.fromSeq(Seq(<n id="1">First</n>,
        <n id="1">Second</n>)))
    }
  }
}
