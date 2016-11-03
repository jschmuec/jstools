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

  val colIdx = Map((headers zipWithIndex): _*)

  /**
   * Converts a [[scala.collection.Seq[Option[V]]] into a [[Map[H,V]]] without 
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
      new SeqTable(
        headers :+ header,
        rows zip values map (x => x._1 :+ x._2)
      )
 
  
  override def toString : String = {
    def noneToNA( o : Option[V] ) : Any = o.getOrElse( "#N/A" )
    val asList = headers :: rows.map( _.map( noneToNA ) ).toList
    asList.map( _.toString ).mkString("\n")
  }
}

/**
 * A simple [[Seq]]-based implementaiton of [[Table]]
 */
class SeqTable[H,V]( val headers : Seq[H], val rows : Seq[Seq[Option[V]]] ) extends Table[H,V]
