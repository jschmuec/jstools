package com.schmueckers

import scala.util.Success
import scala.util.Failure
import scala.util.Try

package object jstools {
  /** Implements the F# pipe operator.
    *
    * It's not really that useful given that methods
    * can't be called and many functions in scala are just methods.
    */
  implicit class Pipe[A](a: A) {
    def |>[B,X>:A](f: X => B) = f(a) 
  }

  implicit def optionToTry[A]( o : Option[A] ) : Try[A] =o match {
    case Some(a) => Success(a)
    case None    => Failure(new NoSuchElementException)
  }
}