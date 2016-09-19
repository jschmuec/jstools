package com.schmueckers.jstools

/**
 * A definition of a table
 *
 * H : Type of the headers
 * V : Type of the Values
 * 
 * There are many different ways to look at the information in a table. This is just
 * one very simple and abstract version which in turn allows to access the data in
 * more sophisticated ways.
 */
trait Table[H, V] {
  /**
   * A table has a [[Seq]] of headers
   */
  def headers: Seq[H]
  
  /**
   * A table has a [[scala.collections.Seq]] of rows. Not all columns have to be defined so each
   * column value is an [[Option[V]]]
   */
  def rows: Seq[Seq[Option[V]]]

  /**
   * Returns a sub-table that only contains the fields that
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
  
  /**
   * Returns the table as a [[Stream]] of [[Map]]s
   * 
   * This felt like a good idea at the time. Not sure we really need it.
   */
  def mapRows : Stream[Map[H,V]] = {
    val colIdx = Map( (headers zipWithIndex) : _* )
    
    def toRowMap( rowSeq : Seq[Option[V]] ) : Map[H,V] = new Map[H,V]() {
      val row_as_list = rowSeq.toList
      def get(key: H): Option[V] = colIdx.get( key ) flatMap ( row_as_list(_) )
      def elements = (headers zip rowSeq).filter( _._2 != None).map( x => (x._1, x._2.get) )
      def iterator: Iterator[(H,V)] = elements.toIterator
      def +[V1 >: V](kv: (H, V1)): Map[H,V1] = Map( elements : _* ) + kv
      def -(key: H) = Map( elements : _* ) - key 
    }
    
    rows.toStream map toRowMap 
  }
}
