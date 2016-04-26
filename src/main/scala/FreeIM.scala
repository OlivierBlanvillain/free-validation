package free.validation

import cats._
import cats.arrow.NaturalTransformation
import cats.functor.Invariant
import cats.free.Inject
import cats.data.Const

// See https://github.com/typelevel/cats/pull/845

/** Invariant version of a Monoidal. */
@simulacrum.typeclass
trait InvariantMonoidal[F[_]] extends Invariant[F] with Cartesian[F] {
  def pure[A](a: A): F[A]
}

object InvariantMonoidal {
  implicit def constIsInvariantMonoidal[T](implicit M: Monoid[T]): InvariantMonoidal[Const[T, ?]] =
    new InvariantMonoidal[Const[T, ?]] {
      def product[A, B](fa: Const[T, A], fb: Const[T, B]): Const[T, (A, B)] =
        Const(M.combine(fa.getConst, fb.getConst))

      def imap[A, B](fa: Const[T, A])(f: A => B)(g: B => A): Const[T, B] =
        fa.retag[B]

      def pure[A](a: A): Const[T, A] =
        Const.empty
    }
}

/** Invariant Monoidal for Free */
sealed abstract class FreeIM[F[_], A] extends Product with Serializable { self =>
  import FreeIM.{FA, Zip, Imap, lift}

  def imap[B](f: A => B)(g: B => A): FA[F, B] =
    Imap(this, f, g)

  def product[B](fb: FA[F, B]): FA[F, (A, B)] =
    Zip(this, fb)

  /** Interprets/Runs the sequence of operations using the semantics of `InvariantMonoidal[G]` */
  def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit im: InvariantMonoidal[G]): G[A]
  // Note that implementing a concrete `foldMap` here does not work because
  // `Zip extends G[(A, B)]` confuses the type inferance when pattern matching on `this`.

  /** Interpret/run the operations using the semantics of `InvariantMonoidal[F]`. */
  final def fold(implicit F: InvariantMonoidal[F]): F[A] =
    foldMap(NaturalTransformation.id[F])

  /** Interpret this algebra into another InvariantMonoidal */
  final def compile[G[_]](f: F ~> G): FA[G, A] =
    foldMap[FA[G, ?]] {
      new NaturalTransformation[F, FA[G, ?]] {
        def apply[B](fa: F[B]): FA[G, B] = lift(f(fa))
      }
    }
}

object FreeIM {
  type FA[F[_], A] = FreeIM[F, A]

  def inject[G[_], H[_]]: FreeIMInjectCurried[G, H] = new FreeIMInjectCurried

  private[validation] final class FreeIMInjectCurried[F[_], G[_]] {
    def apply[A](fa: F[A])(implicit I: Inject[F, G]): FreeIM[G, A] =
      FreeIM.lift(I.inj(fa))
  }

  case class Pure[F[_], A](a: A) extends FA[F, A] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit im: InvariantMonoidal[G]): G[A] =
      im.pure(a)
  }

  case class Suspend[F[_], A](fa: F[A]) extends FA[F, A] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit im: InvariantMonoidal[G]): G[A] =
      nt(fa)
  }

  case class Zip[F[_], A, B](fa: FA[F, A], fb: FA[F, B]) extends FA[F, (A, B)] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit im: InvariantMonoidal[G]): G[(A, B)] =
      im.product(fa.foldMap(nt), fb.foldMap(nt))
  }

  case class Imap[F[_], A, B](fa: FA[F, A], f: A => B, g: B => A) extends FA[F, B] {
    def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit im: InvariantMonoidal[G]): G[B] =
      im.imap(fa.foldMap(nt))(f)(g)
  }

  def pure[F[_], A](a: A): FA[F, A] =
    Pure(a)

  def lift[F[_], A](fa: F[A]): FA[F, A] =
    Suspend(fa)

  /** `FreeIM[S, ?]` has a FreeIM for any type constructor `S[_]`. */
  implicit def freeInvariant[S[_]]: InvariantMonoidal[FA[S, ?]] =
    new InvariantMonoidal[FA[S, ?]] {
      def pure[A](a: A): FA[S, A] = FreeIM.pure(a)
      def imap[A, B](fa: FA[S, A])(f: A => B)(g: B => A): FA[S, B] = fa.imap(f)(g)
      def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] = fa.product(fb)
    }
}
