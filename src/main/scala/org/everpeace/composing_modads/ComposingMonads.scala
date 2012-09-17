package org.everpeace.composing_modads

import scala.Option

object ComposingMonads extends App{

  // type classes: Functor, Monads, Distributives on two Monads.
  trait Functor[F[_]]{
    def map[A,B](ma:F[A])(f:(A) => B):F[B]
  }
  trait Monad[M[_]] extends Functor[M]{
    def unit[A](a:A):M[A]
    def flatten[A](mm:M[M[A]]):M[A]

    def flatMap[A,B](ma:M[A])(f:(A)=>M[B]) = flatten(map(ma)(f))
  }
  // two monads have to be provided because trait's type parameters can't have context bound.
  trait Distributives[M[_],N[_]]{
    def M:Monad[M]
    def N:Monad[N]
    def swap[A](nm:N[M[A]]):M[N[A]]
  }

  // type class instances
  implicit val OptionMonad = new Monad[Option]{
    def map[A, B](ma: Option[A])(f: (A) => B) = ma.map(f)
    def unit[A](a: A) = Option(a)
    def flatten[A](mm: Option[Option[A]]) = mm.flatMap(identity)
  }
  implicit val ListMonad = new Monad[List]{
    def map[A, B](ma: List[A])(f: (A) => B) = ma.map(f)
    def unit[A](a: A) = List(a)
    def flatten[A](mm: List[List[A]]) = mm.flatten
  }
  implicit val ListOptionDistributives = new Distributives[List,Option]{
    def M = implicitly[Monad[List]]
    def N = implicitly[Monad[Option]]
    def swap[A](nm: Option[List[A]]) = nm.getOrElse(List()).map(Option(_))
  }

  // monad composition by distributives.
  implicit def monadsCompose[M[_],N[_]](implicit m:Monad[M], n:Monad[N], s:Distributives[M,N])=
    new Monad[({type MN[α] = M[N[α]]})#MN]{
      def map[A,B](ma:M[N[A]])(f: (A) => B) = m.map(ma)(n.map(_)(f))
      def unit[A](a: A) =  m.unit(n.unit(a))
      def flatten[A](mnmn:M[N[M[N[A]]]])= m.map(m.flatten(m.map(mnmn)(s.swap(_))))(n.flatten(_))
    }

  // provides map/flatmap syntax for monads.
  case class Mval[M[_]:Monad,A](v:M[A]){
    def map[B](f:(A)=>B) = implicitly[Monad[M]].map(v)(f)
    def flatMap[B](f:(A)=>M[B])= implicitly[Monad[M]].flatMap(v)(f)
  }
  def compose[M[_],N[_],A](mna:M[N[A]])
                          (implicit mn:Monad[({type MN[α] = M[N[α]]})#MN])
                          :Mval[({type MN[α] = M[N[α]]})#MN,A]
  = Mval[({type MN[α] = M[N[α]]})#MN,A](mna)

  // i is 1 because *composed* List[Option[_]] monad is active.
  for { i<- compose(List(Option(1)))} yield {println(i);i}

  // i is Some(1) because standard List monad is active.
  for { i<- List(Option(1))         } yield {println(i);i}

  // List[Option[_]] monad computations.
  // this outputs: List(Some(4), Some(5), Some(8), Some(9))
  println(for { i<- compose(List(Option(1), None, Option(5)))
                j<- compose(List(Option(3), Option(4)))     } yield i+j)

}
