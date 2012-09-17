package org.everpeace.composing_modads

import org.everpeace.composing_modads.ComposingMonads.Monad

trait Validation[A]

case class Ok[A](a: A) extends Validation[A]

case class Error[A](msg: String) extends Validation[A]

object Validation {
  def ok[A](a:A):Validation[A] = Ok(a)
  def error[A](msg:String):Validation[A]=Error[A](msg)

  implicit val ValidationMonad = new Monad[Validation] {
    def unit[A](a: A) = Ok(a)
    def map[A, B](ma: Validation[A])(f: (A) => B) = ma match {
      case Ok(a) => Ok(f(a))
      case Error(msg) => Error(msg)
    }
    def flatten[A](mm: Validation[Validation[A]]) = mm match {
      case Ok(a) => a
      case Error(msg) => Error(msg)
    }
  }
}