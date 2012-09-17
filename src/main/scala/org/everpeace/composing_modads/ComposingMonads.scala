package org.everpeace.composing_modads

object ComposingMonads extends App{

  // type classes: Functor, Monads, Distributives on two Monads.
  trait Functor[F[_]]{
    def map[A,B](ma:F[A])(f:(A) => B):F[B]
  }
  trait Monad[M[_]] extends Functor[M]{
    def unit[A](a:A):M[A]
    def flatten[A](mm:M[M[A]]):M[A]
    // flatMap is derived from map and flatten.
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
  implicit def v2M[M[_]:Monad, A](v:M[A]) = Mval(v)
  def compose[M[_],N[_],A](mna:M[N[A]])
                          (implicit mn:Monad[({type MN[α] = M[N[α]]})#MN])
                          :Mval[({type MN[α] = M[N[α]]})#MN,A]
  = Mval[({type MN[α] = M[N[α]]})#MN,A](mna)


  /*******************************/
  /* Sample of Monad Composition */
  /*******************************/

  /* M[Option[_]] can compose. */
  implicit def MonadOptionDistributives[M[_]](implicit monadM: Monad[M]) = new Distributives[M,Option]{
    def M = monadM
    def N = implicitly[Monad[Option]]
    def swap[A](nm: Option[M[A]]) = nm match{
      case Some(ma) => monadM.map(ma)(Option(_))
      case None => monadM.unit(None)
    }
  }
  // i is 1 because *composed* List[Option[_]] monad is active.
  for { i<- compose(List(Option(1)))} yield {println(i);i}

  // i is Some(1) because standard List monad is active.
  for { i<- List(Option(1))         } yield {println(i);i}

  // List[Option[_]] monad computations.
  // this outputs: List(Some(4), Some(5), Some(8), Some(9))
  println(for { i<- compose(List(Option(1), None, Option(5)))
                j<- compose(List(Option(3), Option(4)))     } yield i+j)



  /* M[List[_]] can compose. */
  implicit def MonadListDistributives[M[_]](implicit monadM:Monad[M]) = new Distributives[M,List] {
    def M = monadM
    def N = implicitly[Monad[List]]
    def swap[A](nm: List[M[A]]):M[List[A]] = nm match {
      case List() => monadM.unit(List())
      case x::xs => for { y <- x;
                          ys <- swap(xs)} yield y::ys
    }
  }
  // i is 1 because *composed* Option[List[_]] monad is active.
  for { i<- compose(Option(List(1)))} yield {println(i);i}

  // i is List(1) because standard Option monad is active.
  for { i<- Option(List(1))         } yield {println(i);i}

  // Option[List[_]] monad computations.
  // this outputs: Some(List(4, 5, 8, 9))
  println(for { i<- compose(Option(List(1, 5)))
                j<- compose(Option(List(3, 4)))     } yield i+j)


  /* M[Validation[_]] can compose. */
  import Validation._
  implicit def MonadValidationDistributives[M[_]](implicit monadM:Monad[M]) = new Distributives[M,Validation] {
    def M = monadM
    def N = implicitly[Monad[Validation]]
    def swap[A](nm: Validation[M[A]]) = nm match{
      case Ok(a) => monadM.map(a)(Ok(_))
      case Error(msg) => monadM.unit(Error(msg))
    }
  }
  // this outputs: List(Ok(3), Error(error2), Error(error3), Error(error1), Ok(4), Error(error2), Error(error3))
  println(for { i <- compose(List(ok(1),error[Int]("error1"),Ok(2)));
                j <- compose(List(ok(2),error[Int]("error2"),error[Int]("error3")))} yield i+j )

}
