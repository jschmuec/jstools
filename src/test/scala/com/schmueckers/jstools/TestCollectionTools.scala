package com.schmueckers.jstools

import scala.xml.Null

import org.scalatest.FunSpec
import org.scalatest.Matchers
import com.schmueckers.jstools.CollectionTools._

/** Tests for [[MapTable]]
  */
class TestCollectionTools extends FunSpec with Matchers {
  describe("The ReverseIterable implicit") {
    it("should add reverse to any Iterable") {
      val l = List(1, 2, 3, 4)
      val iterable: Iterable[Int] = l
      iterable.reverse should be(l.reverse)
    }
    it("should handle an empty Iterable correctly") {
      val l = List()
      val iterable: Iterable[Nothing] = l
      iterable.reverse.size should be(0)
    }
  }
}