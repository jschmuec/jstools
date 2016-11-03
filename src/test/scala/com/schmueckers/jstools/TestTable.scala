package com.schmueckers.jstools

import org.scalatest.FunSpec
import org.scalatest.Matchers

/**
 * Tests for [[MapTable]]
 */
class TestTable extends FunSpec with Matchers {
  describe("Table") {
    val keys = ('A' to 'C').toSeq

    def generate_row[K, V, T](keys: Seq[K], value: V, join: (K, V) => T) =
      keys.map(key => Some(join(key, value)))

    def generate_rows[K, V, T](keys: Seq[K], values: Seq[V], join: (K, V) => T) =
      values.map(generate_row(keys, _, join))

    val join = (c: Char, i: Int) => s"$c-$i"

    it("Should allow to add a column") {
      val table = new SeqTable(keys, generate_rows(keys, 0 to 10, join))
      table.addColumn('D', generate_row("D", 11, join))
    }
  }

}
