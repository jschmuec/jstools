package com.schmueckers.jstools

/** A definition of a table
  *
  * H : Type of the headers
  * V : Type of the Values
  *
  * There are many different ways to look at the information in a table. This is just
  * one very simple and abstract version which in turn allows to access the data in
  * more sophisticated ways.
  */
trait Table[H, V] {
  outer =>
  /** A table has a [[Seq]] of headers
    */
  def headers: Seq[H]

  /** A table has a [[scala.collection.Seq]] of rows. Not all columns have to be defined so each
    * column value is an [[Option[V]]]
    */
  def rows: Seq[Seq[Option[V]]]

  /** Returns a sub-table that only contains the fields that
    * are defined in the [[fields]] parameter in the defined
    * sequence
    */
  def sub(fields: Seq[H]) = {
    val included_idx = fields.map(headers.indexOf(_))
    val filtered_rows = rows.map(r => included_idx.map(r(_)))
    new Table[H, V]() {
      val headers = fields
      val rows = filtered_rows
    }
  }

  /** Returns the table as a [[Stream]] of [[Map]]s
    *
    * This felt like a good idea at the time. Not sure we really need it.
    */
  def mapRows: Stream[Map[H, V]] = {

    rows.toStream map rowToMap
  }

  val colIdx = {
    import scala.language.postfixOps

    Map((headers zipWithIndex): _*)
  }

  /** Converts a [[scala.collection.Seq[Option[V]]] into a [[Map[H,V]]] without
    * copying the data over into a Map
    */
  private def rowToMap(rowSeq: Seq[Option[V]]): Map[H, V] = new Map[H, V]() {
    val row_as_list = rowSeq.toList
    def get(key: H): Option[V] = colIdx.get(key) flatMap (row_as_list(_))
    def elements = (headers zip rowSeq).filter(_._2 != None).map(x => (x._1, x._2.get))
    def iterator: Iterator[(H, V)] = elements.toIterator
    def +[V1 >: V](kv: (H, V1)): Map[H, V1] = Map(elements: _*) + kv
    def -(key: H) = Map(elements: _*) - key
  }

  def filter(f: Map[H, V] => Boolean) = new Table[H, V] {
    def headers: Seq[H] = outer.headers
    def rows: Seq[Seq[Option[V]]] = (outer.rows map rowToMap filter f) map (_.values.toSeq.map(Some(_)))
  }

  def addColumn(header: H, values: Iterable[Option[V]]) =
    Table(
      headers :+ header,
      rows zip values map (x => x._1 :+ x._2))

  private def join[K, X, VA <: X, VB <: X](a: Table[K, VA], b: Table[K, VB]): Table[K, X] = {
    val headers = a.headers ++ b.headers.filterNot(a.headers.contains(_))
    val rows = a.mapRows ++ b.mapRows
    new MapTable(rows, headers)
  }

  /** Adds a row to the Table.
    * The Table will change into a {{MapTable}} but that shouldn't make a difference
    * as it still is a Table.
    * @param row The row to be added.
    *
    * Note: The types could probably be relaxed as any subtype of H and V
    * should be OK to be added.
    */
  def +[VB <: V](row: Map[H, VB]): Table[H, V] = new MapTable(this.mapRows) + row

  /** Adds all the elements of another table to this table.
    * The joined Table will have the fields of this first followed by the additional
    * fields from the joined table.
    *
    * The implementation uses a helper method join because I couldn't get the ctypes toiteratorwork.
    */
  def ++[VJ >: V, VB <: VJ](b: Table[H, VB]): Table[H, VJ] = join(this, b)

  override def toString: String = {
    def noneToNA(o: Option[V]): Any = o.getOrElse("#N/A")
    val asList = headers :: rows.map(_.map(noneToNA)).toList
    asList.map(_.toString).mkString("\n")
  }
}

object Table {
  /** Creates a [[Table]] from rows that are sequences of keys and option field values
    */
  def apply[H, V](hs: Seq[H], rs: Seq[Seq[Option[V]]]): Table[H, V] = new Table[H, V] {
    def headers = hs
    def rows = rs
  }
  /** Create a [[Table]] from rows that are provided as maps.
    *
    * @headers: An optional sequence of headers that defines the order of columns for this table
    */
  def apply[H, V](maps: Iterable[Map[H, V]], headers: Option[Seq[H]] ): Table[H, V] =
    new MapTable(maps, headers)

  def apply[H, V](maps: Iterable[Map[H, V]], headers: Seq[H] ): Table[H, V] =
    new MapTable(maps, Some(headers))

  def apply[H, V](maps: Iterable[Map[H, V]] ): Table[H, V] =
    new MapTable(maps, None )
}

