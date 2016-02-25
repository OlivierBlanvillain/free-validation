package free.validation

object Algebra {
  type Dsl[A] = FreeInvariantMonoidal[AL, A]
  
  // To be expended with Null Boolean and Seq, option should maybe be added in the Algebra.
  
  sealed trait AL[A]
  case class IntAL[A](field: String) extends AL[Int]
  case class StringAL[A](field: String) extends AL[String]
  case class BooleanAL[A](field: String) extends AL[Boolean]
  case class ShortAL[A](field: String) extends AL[Short]
  case class LongAL[A](field: String) extends AL[Long]
  case class FloatAL[A](field: String) extends AL[Float]
  case class BigDecimalAL[A](field: String) extends AL[BigDecimal]
  case class DoubleAL[A](field: String) extends AL[Double]
  
  case class ObjectAL[A](field: String, value: Dsl[A]) extends AL[A]
  case class EnsureAL[A](e: A => Boolean, r: A => String, value: Dsl[A]) extends AL[A]
  case class OptionalAL[A](value: Dsl[A]) extends AL[A]
  
  case class SeqAl[A](value: Dsl[A]) extends AL[Seq[A]]
}

object Dsl {
  import Algebra._
  import FreeInvariantMonoidal.lift
  type Dsl[A] = FreeInvariantMonoidal[AL, A]
  
  def int(field: String): Dsl[Int] = lift[AL, Int](IntAL(field))
  def string(field: String): Dsl[String] = lift[AL, String](StringAL(field))
  def boolean(field: String): Dsl[Boolean] = lift[AL, Boolean](BooleanAL(field))
  def short(field: String): Dsl[Short] = lift[AL, Short](ShortAL(field))
  def long(field: String): Dsl[Long] = lift[AL, Long](LongAL(field))
  def float(field: String): Dsl[Float] = lift[AL, Float](FloatAL(field))
  def bigdecimal(field: String): Dsl[BigDecimal] = lift[AL, BigDecimal](BigDecimalAL(field))
  def double(field: String): Dsl[Double] = lift[AL, Double](DoubleAL(field))
  
  def obj[A](field: String)(value: Dsl[A]): Dsl[A] = lift(ObjectAL(field, value))
  def ensure[A](e: A => Boolean, r: A => String)(value: Dsl[A]): Dsl[A] = lift(EnsureAL(e, r, value))
  def optional[A](value: Dsl[A]): Dsl[Option[A]] =
    (lift(OptionalAL(value)): Dsl[A]).imap(Option.apply)(_.get)
    
  def seq[A](value: Dsl[A]): Dsl[Seq[A]] = lift[AL, Seq[A]](SeqAl(value))
}

object Helpers {
  import Dsl._
  import play.api.data.mapping.VA
  
  def min(i: Int)(value: Dsl[Int]): Dsl[Int] = {
    def msg(j: Int) = s"$j is above the $i"
    ensure[Int](i.>=, msg)(value)
  }
  
  def max(i: Int)(value: Dsl[Int]): Dsl[Int] = {
    def msg(j: Int) = s"$j is below $i"
    ensure[Int](i.<=, msg)(value)
  }
  
  def nonEmpty(value: Dsl[String]): Dsl[String] = {
    def msg(s: String) = s"Empty string"
    ensure[String](_.nonEmpty, msg)(value)
  }
  
  def increasing[A](value: Dsl[(A, A)])(implicit o: Ordering[A]): Dsl[(A, A)] = {
    def msg(p: (A, A)) = s"$p is not increating"
    ensure[(A, A)](Function.tupled(o.lt), msg)(value)
  }
  
  // The `???` and the `.get` are safe if the compiler handle `ensure` as expected.
  def iflatMap[A, B](f: A => VA[B], g: B => A)(value: Dsl[A]): Dsl[B] =
    (value % ensure[A](a => f(a).isSuccess, a => f(a).fold(_ => "???", _.toString)))
      .imap(a => f(a).get)(g)
}
