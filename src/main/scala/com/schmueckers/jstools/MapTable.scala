package com.schmueckers.jstools

import com.schmueckers.jstools.Table
import scala.collection.mutable.SetBuilder
import scala.annotation.migration
import scala.annotation.tailrec

/**
 * A implementation of [[Table]] which takes a list of rows as constructor
 */
class MapTable[K,V]( private val map_rows: Iterable[Map[K,V]] ) extends Table[K,V]{
  def row_to_seq( m : Map[K,V] ) : Seq[Option[V]] = 
    headers.map( m.get( _ ) )
    
  def rows: Seq[Seq[Option[V]]] = map_rows.map( row_to_seq ).toSeq
  
  /**
   * Returns a [[Seq]] of all the key values in the input Maps
   */
  lazy val headers : Seq[K] = {
    val all_keys = map_rows.toStream.flatMap( _.keys )
    val sb = new SetBuilder[K,Set[K]]( Set.empty[K] )
    all_keys.foreach( sb += _ )
    val result = sb.result
    result.toSeq
  } 
}