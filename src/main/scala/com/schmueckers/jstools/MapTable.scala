package com.schmueckers.jstools

import scala.collection.mutable.SetBuilder

/** A implementation of [[Table]] which takes a list of rows as constructor
  *
  * @param map_rows The rows of the table as map
  * @param o_headers An optional sequence of headers that will be used. If this parameters is not
  * provided, the random order of the joined set of all the keys of the rows will be used.
  */
class MapTable[K, V](val map_rows: Iterable[Map[K, V]], o_headers: Option[Seq[K]] = None) extends Table[K, V] {
  /** A constructor with a Seq that defines the headers. Convenience to make
    * creating MapTables with given headers easier.
    */
  def this(map_rows: Iterable[Map[K, V]], headers: Seq[K]) = this(map_rows, headers match {
    case Nil     => None
    case headers => Some(headers)
  })

  def row_to_seq(m: Map[K, V]): Seq[Option[V]] =
    headers.map(m.get(_))

  def rows: Seq[Seq[Option[V]]] = map_rows.map(row_to_seq).toSeq

  /** Returns a [[Seq]] of all the key values in the input Maps
    */
  lazy val headers: Seq[K] =
    o_headers.getOrElse {
      val all_keys = map_rows.toStream.flatMap(_.keys)
      val sb = new SetBuilder[K, Set[K]](Set.empty[K])
      all_keys.foreach(sb += _)
      val result = sb.result
      result.toSeq
    }
}