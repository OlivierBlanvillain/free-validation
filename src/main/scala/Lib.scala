package free.validation

import cats.free.Inject
import play.api.data.mapping.Path

object Algebra {
  sealed trait JsonLikeAlgebra[M[_], A]
  type CA[M[_], A] = JsonLikeAlgebra[M, A]
  case class IntAL        [M[_]](path: Path, mark: Seq[M[Int]])        extends CA[M, Int]
  case class StringAL     [M[_]](path: Path, mark: Seq[M[String]])     extends CA[M, String]
  case class BooleanAL    [M[_]](path: Path, mark: Seq[M[Boolean]])    extends CA[M, Boolean]
  case class ShortAL      [M[_]](path: Path, mark: Seq[M[Short]])      extends CA[M, Short]
  case class LongAL       [M[_]](path: Path, mark: Seq[M[Long]])       extends CA[M, Long]
  case class FloatAL      [M[_]](path: Path, mark: Seq[M[Float]])      extends CA[M, Float]
  case class BigDecimalAL [M[_]](path: Path, mark: Seq[M[BigDecimal]]) extends CA[M, BigDecimal]
  case class DoubleAL     [M[_]](path: Path, mark: Seq[M[Double]])     extends CA[M, Double]

  case class ObjectAL     [M[_], A](path: Path, value: FreeIM[CA[M, ?], A], marks: Seq[M[A]]) extends CA[M, A]
  case class OptionalAL   [M[_], A](path: Path, value: FreeIM[CA[M, ?], A]) extends CA[M, Option[A]]
  case class SeqAl        [M[_], A](path: Path, value: FreeIM[CA[M, ?], A]) extends CA[M, Seq[A]]

  sealed trait DefaultMarks[A]
  case class NonEmptyM[A]() extends DefaultMarks[A]
  case class MinM[A](value: Int)(implicit val n: Numeric[A]) extends DefaultMarks[A]
  case class MaxM[A](value: Int)(implicit val n: Numeric[A]) extends DefaultMarks[A]
}

object Dsl {
  import Algebra._

  class JsonLikeDsl[M[_]](implicit I: Inject[DefaultMarks, M]) extends DefaultMarksDsl[M](I) with PlayStyleDsl[M] {
    type AL[A] = JsonLikeAlgebra[M, A]
    
    private def lift[A] = FreeIM.lift[AL, A] _

    implicit def int(path: Path)(marks: Seq[M[Int]]): FreeIM[AL, Int] = lift(IntAL(path, marks))
    implicit def string(path: Path)(marks: Seq[M[String]]): FreeIM[AL, String] = lift(StringAL(path, marks))
    implicit def boolean(path: Path)(marks: Seq[M[Boolean]]): FreeIM[AL, Boolean] = lift(BooleanAL(path, marks))
    implicit def short(path: Path)(marks: Seq[M[Short]]): FreeIM[AL, Short] = lift(ShortAL(path, marks))
    implicit def long(path: Path)(marks: Seq[M[Long]]): FreeIM[AL, Long] = lift(LongAL(path, marks))
    implicit def float(path: Path)(marks: Seq[M[Float]]): FreeIM[AL, Float] = lift(FloatAL(path, marks))
    implicit def bigd(path: Path)(marks: Seq[M[BigDecimal]]): FreeIM[AL, BigDecimal] = lift(BigDecimalAL(path, marks))
    implicit def double(path: Path)(marks: Seq[M[Double]]): FreeIM[AL, Double] = lift(DoubleAL(path, marks))
    
    implicit def obj[A](path: Path)(marks: Seq[M[A]])(implicit value: FreeIM[AL, A]): FreeIM[AL, A] =
      lift(ObjectAL(path, value, marks))

    // Marks' not used in those two 
    implicit def seq[A](path: Path)(marks: Seq[M[A]])(implicit value: FreeIM[AL, A]): FreeIM[AL, Seq[A]] =
      lift(SeqAl(path, value))

    implicit def opt[A](path: Path)(marks: Seq[M[Option[A]]])(implicit value: FreeIM[AL, A]): FreeIM[AL, Option[A]] =
      lift(OptionalAL(path, value))
  }
  
  object JsonLikeDsl {
    implicit def injectJsonLikeDsl[N[_]](implicit I: Inject[DefaultMarks, N]): JsonLikeDsl[N] = new JsonLikeDsl[N]
  }
  
  class DefaultMarksDsl[M[_]](I: Inject[DefaultMarks, M]) {
    def nonEmpty[A]: M[A] = I.inj(NonEmptyM())
    def min[A: Numeric](value: Int): M[A] = I.inj(MinM(value))
    def max[A: Numeric](value: Int): M[A] = I.inj(MaxM(value))
  }
  
  trait PlayStyleDsl[M[_]] {
    type JLAL[A] = JsonLikeAlgebra[M, A]
    
    implicit val implicitSearchHint: InvariantMonoidal[FreeIM[JLAL, ?]] =
      FreeIM.freeInvariant[JLAL]
    
    implicit class RichPath(path: Path) {
      def as[T] = new RichPathCurried[T](path)
    }
    
    class RichPathCurried[T](path: Path) {
      def apply(marks: M[T]*)(implicit al: Path => Seq[M[T]] => FreeIM[JLAL, T]): FreeIM[JLAL, T] = al(path)(marks)
      def apply()(implicit al: Path => Seq[M[T]] => FreeIM[JLAL, T]): FreeIM[JLAL, T] = al(path)(Seq())
    }
    
    val __ = Path
  }
}
