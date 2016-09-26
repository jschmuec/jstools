package com.schmueckers.jstools
object CollectionTools {
  implicit class MapContains[K, V](m: Map[K, V]) {
    def contains(kvp: (K, V)) = m.get(kvp._1) match {
      case Some(v) => v == kvp._2
      case None    => false
    }
    def not[T]( p: (T) => Boolean ) : (T) => Boolean = (t : T) => ! p(t)  
    def containsAll(s: Map[K, V]) = s.find( not( contains ) )
  }
}