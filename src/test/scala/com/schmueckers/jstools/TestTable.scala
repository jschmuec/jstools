package com.schmueckers.jstools

import org.scalatest.{Matchers, FunSpec, GivenWhenThen}

/**
 * Tests for [[MapTable]]
 */
class TestTable extends FunSpec with Matchers with GivenWhenThen {
  describe("Table") {
    val keys = ('A' to 'C').toSeq

    def generate_row[K, V, T](keys: Seq[K], value: V, join: (K, V) => T) =
      keys.map(key => Some(join(key, value)))

    def generate_rows[K, V, T](keys: Seq[K], values: Seq[V], join: (K, V) => T) =
      values.map(generate_row(keys, _, join))

    val join = (c: Char, i: Int) => s"$c-$i"

    it("Should allow to add a column") {
      val table = Table(keys, generate_rows(keys, 0 to 10, join))
      val newTable = table.addColumn('D', (0 to 10).map(i => Some(join('D', i))))

      newTable.headers should be('A' to 'D')
      val rows = newTable.rows
      rows(0) should be(generate_row('A' to 'D', 0, join))
    }
    it("Should be able to add another table") {
      val keys = 'A' to 'D'
      val keys1 = keys.take(3)
      val rows1 = generate_rows(keys1, 0 to 10, join)
      val keys2 = keys.drop(1)
      val rows2 = generate_rows(keys2, 11 to 20, join)

      Given(s"A table with columns ${keys1}")
      val table1 = Table(keys1, rows1)
      Given(s"Another table with columns ${keys2}")
      val table2 = Table(keys2, rows2)

      When("Joining these tables")
      val joined = table1 ++ table2

      Then("The joined headers should be ${keys}")
      joined.headers should be(keys)

      Then("The first 3 columns of the first 11 rows of the joined table should be equal to the rows of the first table")
      joined.rows.map(_.take(3)).take(11) should equal(rows1)

      Then("The last 3 columns of the last 11 rows of the joined table should be equal to the rows of the second table")
      joined.rows.map(_.drop(1)).drop(11) should equal(rows2)
    }
    it("should add a new row when a new map is addeed and add the new headers at the end") {
      val row1 = Map("A" -> "A1", "B" -> "B1", "C" -> "C1")
      val new_row = Map("A" -> "AN", "C" -> "CN", "D" -> "DN")
      val table = new MapTable(List(row1))
      val new_table = table + new_row

      new_table.mapRows(0) should be(row1)
      new_table.mapRows(1) should be(new_row)
      new_table.headers should be(List("A", "B", "C", "D"))
    }
    it("should fail if I try to call fromComplete with incorrect number of columns in the data") {
      assertThrows[AssertionError] {
        Table.fromComplete( "a" :: "b" :: Nil, List( "A" :: "B" :: Nil, "A2" :: Nil ) )
      }
    }
    it("should create a correct table given complete data using Table.fromComplete") {
      Table.fromComplete( "a" :: "b" :: Nil, List( "A1" :: "B1" :: Nil, "A2" :: "B2" :: Nil ))
    }
  }
}
