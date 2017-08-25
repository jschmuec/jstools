package com.schmueckers.jstools

import java.util.Random

/**
  * Various methods to create streams of random numbers
  *
  * This is a fully functional implementation because as long as you
  * keep track of the head of the stream it will just be there.
  */
object RandomStream {
  /**
    * Returns a stream of random numbers
    *
    * A function which actually repeatedly calls the method passed
    * in on the random number generator and thereby creates a stream
    * of random elements of the given type. In practice use the Int()
    * and Int( bound : Int ) methods to get a stream of the right
    * type.
    *
    * Do not hold the head of the stream as this would stop the GC.
    *
    * @param f The function on java.util.Random that should be invoked to pull
    *          the next element out of the generator
    * @tparam T The type of the value to be returned in the stream
    */
  def create[T](f: (Random) => T) = {
    val rand = new java.util.Random

    def get_next: Stream[T] =
      f(rand) #:: get_next
    get_next
  }

  // convenience methods and sample implementations
  def Int = create( (r : java.util.Random ) => r.nextInt )
  def BoundInt( bound : Int ) = create( (r: java.util.Random) => r.nextInt( bound ) )
}
