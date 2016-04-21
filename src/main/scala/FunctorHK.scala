package free.validation

import Algebra.CoreAlgebra

trait FunctorHK[F[_[_]]] {
  def map[A[_], B[_], T](fa: F[A])(f: A[T] => B[T]): F[B]
}

object FunctorHK {
  implicit def coreAlgebraInstance[X]: FunctorHK[Lambda[l[_] => CoreAlgebra[l, X]]] =
    new FunctorHK[Lambda[l[_] => CoreAlgebra[l, X]]] {
      def map[A[_], B[_], T](fa: CoreAlgebra[A, X])(f: A[T] => B[T]): CoreAlgebra[B, X] =
        fa.asInstanceOf[CoreAlgebra[B, X]] // dats gonna end as bytecode anyways lol
    }
}
