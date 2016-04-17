package free.validation

import cats.free.Inject
import play.api.data.mapping.Path

object Algebra {
 sealed trait CoreAlgebra[A]
  case class IntAL       [A](path: Path) extends CoreAlgebra[Int]
  case class StringAL    [A](path: Path) extends CoreAlgebra[String]
  case class BooleanAL   [A](path: Path) extends CoreAlgebra[Boolean]
  case class ShortAL     [A](path: Path) extends CoreAlgebra[Short]
  case class LongAL      [A](path: Path) extends CoreAlgebra[Long]
  case class FloatAL     [A](path: Path) extends CoreAlgebra[Float]
  case class BigDecimalAL[A](path: Path) extends CoreAlgebra[BigDecimal]
  case class DoubleAL    [A](path: Path) extends CoreAlgebra[Double]

  case class ObjectAL    [A](path: Path, value: FreeIM[CoreAlgebra, A]) extends CoreAlgebra[A]
  case class SeqAl       [A](path: Path, value: FreeIM[CoreAlgebra, A]) extends CoreAlgebra[Seq[A]]
  case class OptionalAL  [A](value: FreeIM[CoreAlgebra, A])             extends CoreAlgebra[A]

  case class EnsureAL[A](
    condition: A => Boolean,
    message: A => String,
    value: FreeIM[CoreAlgebra ,A]
  ) extends CoreAlgebra[A]
}

object Dsl {
  import Algebra._

  class CoreDsl[F[_]](implicit I: Inject[CoreAlgebra, F]) {
    private def lift[A](a: CoreAlgebra[A]):  FreeIM[F, A] = FreeIM.inject[CoreAlgebra, F](a)

    implicit def int       (path: Path): FreeIM[F, Int] =        lift(IntAL(path))
    implicit def string    (path: Path): FreeIM[F, String] =     lift(StringAL(path))
    implicit def boolean   (path: Path): FreeIM[F, Boolean] =    lift(BooleanAL(path))
    implicit def short     (path: Path): FreeIM[F, Short] =      lift(ShortAL(path))
    implicit def long      (path: Path): FreeIM[F, Long] =       lift(LongAL(path))
    implicit def float     (path: Path): FreeIM[F, Float] =      lift(FloatAL(path))
    implicit def bigdecimal(path: Path): FreeIM[F, BigDecimal] = lift(BigDecimalAL(path))
    implicit def double    (path: Path): FreeIM[F, Double] =     lift(DoubleAL(path))

    implicit def obj[A](path: Path)(implicit value: FreeIM[CoreAlgebra, A]): FreeIM[F, A] =
      lift(ObjectAL(path, value))

    implicit def seq[A](path: Path)(implicit value: FreeIM[CoreAlgebra, A]): FreeIM[F, Seq[A]] =
      lift(SeqAl(path, value))

    def optional[A](path: Path)(value: FreeIM[CoreAlgebra, A]): FreeIM[F, A] =
      lift(OptionalAL(value))

    def ensure[A](condition: A => Boolean, message: A => String)(value: FreeIM[CoreAlgebra, A]): FreeIM[F, A] =
      lift(EnsureAL(condition, message, value))
  }

  object CoreDsl {
    implicit def injectCoreDsl[F[_]](implicit I: Inject[CoreAlgebra, F]): CoreDsl[F] = new CoreDsl[F]
    object playStyle extends CoreDsl[CoreAlgebra] with PlayStyleDsp[CoreAlgebra]
    object freeStyle extends CoreDsl[CoreAlgebra]
  }

  trait PlayStyleDsp[F[_]] {
    val __ = Path

    implicit class RichPath(path: Path) {
      def as[T](implicit al: Path => FreeIM[F, T]): FreeIM[F, T] = al(path)
    }
  }
}

object Helpers {
  import Algebra.CoreAlgebra
  import Dsl.CoreDsl.freeStyle._
  import play.api.data.mapping.VA

  def min(i: Int)(value: FreeIM[CoreAlgebra, Int]): FreeIM[CoreAlgebra, Int] = {
    def msg(j: Int) = s"$j is above the $i"
    ensure[Int](i.>=, msg)(value)
  }

  def max(i: Int)(value: FreeIM[CoreAlgebra, Int]): FreeIM[CoreAlgebra, Int] = {
    def msg(j: Int) = s"$j is below $i"
    ensure[Int](i.<=, msg)(value)
  }

  def nonEmpty(value: FreeIM[CoreAlgebra, String]): FreeIM[CoreAlgebra, String] = {
    def msg(s: String) = s"Empty string"
    ensure[String](_.nonEmpty, msg)(value)
  }

  def increasing[A](value: FreeIM[CoreAlgebra, (A, A)])(implicit o: Ordering[A]): FreeIM[CoreAlgebra, (A, A)] = {
    def msg(p: (A, A)) = s"$p is not increating"
    ensure[(A, A)](Function.tupled(o.lt), msg)(value)
  }

  // The `???` and the `.get` are safe if the compiler handle `ensure` as expected.
  def iflatMap[A, B](f: A => VA[B], g: B => A)(value: FreeIM[CoreAlgebra, A]): FreeIM[CoreAlgebra, B] =
    (value % ensure[A](a => f(a).isSuccess, a => f(a).fold(_ => "???", _.toString)))
      .imap(a => f(a).get)(g)
}
