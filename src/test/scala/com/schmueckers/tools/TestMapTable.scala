package com.schmueckers.jstools

import scala.xml.Null

import org.scalatest.FunSpec
import org.scalatest.Matchers
import com.schmueckers.jstools.Pipe

import com.schmueckers.jstools.optionToTry
import com.schmueckers.jstools.MapTable

/**
 * Tests for [[MapTable]]
 */
class TestMapTable extends FunSpec with Matchers {
  describe("MapTable") {
    val keys = Set("A", "B", "C")

    def keys_to_map(keys: Iterable[String]) = keys.zip(keys).toMap
    val subset_maps = keys.subsets().toList.map(keys_to_map)

    val subset_table = new MapTable(subset_maps)

    it("should return the right number of rows") {
      val count = 5
      val mp = new MapTable((1 to count).map(x => Map("A" -> 1, "B" -> 2)))
      mp.rows.size should be(5)
    }
    it("The keys should be the join over all keys") {
      subset_table.headers.toSet should be(keys)
    }
    it("should return the correct value or Option for each row and header") {
      for {
        row_i <- 0 to subset_table.rows.size - 1
        header_i <- 0 to subset_table.headers.size - 1
      } {
        subset_table.rows(row_i)(header_i) should be
        (subset_maps(row_i).get(subset_table.headers(header_i)))
      }
    }
  }
}