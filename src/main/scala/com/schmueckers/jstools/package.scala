package com.schmueckers

package object jstools {
  /**
   * Implements the F# pipe operator. 
   * 
   * It's not really that useful given that methods
   * can't be called and many functions in scala are just methods.
   */
  implicit class Pipe[A](a: A) {
    def |>[B](f: A => B) = f(a)
  }
}