package com.schmueckers.jstools

import org.scalatest.FunSpec
import org.scalatest.Matchers

/** Tests for [[MapTable]]
  */
class TestMapTable extends FunSpec with Matchers {
  describe("MapTable") {
    val keys = Set("A", "B", "C")

    def keys_to_map(keys: Iterable[String]) = keys.zip(keys).toMap
    val subset_maps = keys.subsets().toList.map(keys_to_map)

    val subset_table = Table(subset_maps)

    it("should return the right number of rows") {
      val count = 5
      val mp = Table((1 to count).map(x => Map("A" -> 1, "B" -> 2)))
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
    it(
      """should return the fields of the row Maps in the same sequence as the
          headers provided.""") {
        val mp = new MapTable(subset_maps, List("C", "A"))
        mp.headers should be(List("C", "A"))
        for {
          row_i <- 0 to subset_table.rows.size - 1
          r = mp.rows(row_i)
          map = subset_maps(row_i)
          header_i <- 0 to 1
          h = mp.headers(header_i)
        } {
          r(header_i) should be(map.get(h))
        }
      }
  }
}
