import cats._
import cats.functor.Invariant
import simulacrum.typeclass

/** Invariant version of a Monoidal. */
@typeclass trait InvariantMonoidal[F[_]] extends Invariant[F] with Cartesian[F] {
  def pure[A](a: A): F[A]
}
