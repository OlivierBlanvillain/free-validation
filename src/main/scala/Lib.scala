package free.validation

import cats.free.Inject
import play.api.data.mapping.Path

object Algebra {
  sealed trait CoreAlgebra[M[_], A]
  type CA[M[_], A] = CoreAlgebra[M, A]
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

  class CoreDsl[M[_]](implicit I: Inject[DefaultMarks, M]) {
    type CAM[A] = CoreAlgebra[M, A]
    
    private def lift[A] = FreeIM.lift[CAM, A] _

    implicit def int       (path: Path)(marks: Seq[M[Int]]): FreeIM[CAM, Int] =               lift(IntAL(path, marks))
    implicit def string    (path: Path)(marks: Seq[M[String]]): FreeIM[CAM, String] =         lift(StringAL(path, marks))
    implicit def boolean   (path: Path)(marks: Seq[M[Boolean]]): FreeIM[CAM, Boolean] =       lift(BooleanAL(path, marks))
    implicit def short     (path: Path)(marks: Seq[M[Short]]): FreeIM[CAM, Short] =           lift(ShortAL(path, marks))
    implicit def long      (path: Path)(marks: Seq[M[Long]]): FreeIM[CAM, Long] =             lift(LongAL(path, marks))
    implicit def float     (path: Path)(marks: Seq[M[Float]]): FreeIM[CAM, Float] =           lift(FloatAL(path, marks))
    implicit def bigdecimal(path: Path)(marks: Seq[M[BigDecimal]]): FreeIM[CAM, BigDecimal] = lift(BigDecimalAL(path, marks))
    implicit def double    (path: Path)(marks: Seq[M[Double]]): FreeIM[CAM, Double] =         lift(DoubleAL(path, marks))

    implicit def obj[A](path: Path)(marks: Seq[M[A]])(implicit value: FreeIM[CAM, A]): FreeIM[CAM, A] =
      lift(ObjectAL(path, value, marks))

    // Marks' not used in those two 
    implicit def seq[A](path: Path)(marks: Seq[M[A]])(implicit value: FreeIM[CAM, A]): FreeIM[CAM, Seq[A]] =
      lift(SeqAl(path, value))

    implicit def opt[A](path: Path)(marks: Seq[M[Option[A]]])(implicit value: FreeIM[CAM, A]): FreeIM[CAM, Option[A]] =
      lift(OptionalAL(path, value))
    
    def nonEmpty[A]: M[A] = I.inj(NonEmptyM())
    def min[A: Numeric](value: Int): M[A] = I.inj(MinM(value))
    def max[A: Numeric](value: Int): M[A] = I.inj(MaxM(value))
    
    
    implicit val implicitSearchHint: InvariantMonoidal[FreeIM[Lambda[l => CoreAlgebra[M, l]], ?]] =
      FreeIM.freeInvariant[CoreAlgebra[M, ?]]
      
    // Play style stuff
    
    type AL[T] = CoreAlgebra[M, T]
    
    implicit class RichPath(path: Path) {
      def as[T](implicit al: Path => Seq[M[T]] => FreeIM[AL, T]): FreeIM[AL, T] = al(path)(Seq())
      
      def as[T](mark: M[T])(implicit al: Path => Seq[M[T]] => FreeIM[AL, T]): FreeIM[AL, T] = al(path)(Seq(mark))
    }
    
    val __ = Path
  }

  object CoreDsl {
    implicit def injectCoreDsl[N[_]](implicit I: Inject[DefaultMarks, N]): CoreDsl[N] = new CoreDsl[N]
  }
}

// object Helpers {
//   import Algebra.CoreAlgebra
//   import Dsl.CoreDsl.freeStyle._
//   import play.api.data.mapping.VA

//   def min(i: Int)(value: FreeIM[CoreAlgebra, Int]): FreeIM[CoreAlgebra, Int] = {
//     def msg(j: Int) = s"$j is above the $i"
//     ensure[Int](i.>=, msg)(value)
//   }

//   def max(i: Int)(value: FreeIM[CoreAlgebra, Int]): FreeIM[CoreAlgebra, Int] = {
//     def msg(j: Int) = s"$j is below $i"
//     ensure[Int](i.<=, msg)(value)
//   }

//   def nonEmpty(value: FreeIM[CoreAlgebra, String]): FreeIM[CoreAlgebra, String] = {
//     def msg(s: String) = s"Empty string"
//     ensure[String](_.nonEmpty, msg)(value)
//   }

//   def increasing[A](value: FreeIM[CoreAlgebra, (A, A)])(implicit o: Ordering[A]): FreeIM[CoreAlgebra, (A, A)] = {
//     def msg(p: (A, A)) = s"$p is not increating"
//     ensure[(A, A)](Function.tupled(o.lt), msg)(value)
//   }

//   // The `???` and the `.get` are safe if the compiler handle `ensure` as expected.
//   def iflatMap[A, B](f: A => VA[B], g: B => A)(value: FreeIM[CoreAlgebra, A]): FreeIM[CoreAlgebra, B] =
//     (value % ensure[A](a => f(a).isSuccess, a => f(a).fold(_ => "???", _.toString)))
//       .imap(a => f(a).get)(g)
// }
