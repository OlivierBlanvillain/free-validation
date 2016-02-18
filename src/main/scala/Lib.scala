package free.validation

object Algebra {
  type Dsl[A] = FreeInvariantMonoidal[AL, A]
  
  // To be expended with Null Boolean and Seq, option should maybe be added in the Algebra.
  
  sealed trait AL[A]
  case class IntAL[A](field: String, value: Int => A, cvalue: A => Int) extends AL[A]
  case class StrAL[A](field: String, value: String => A, cvalue: A => String) extends AL[A]
  case class ObjAL[A](field: String, value: Dsl[A]) extends AL[A]
  
  case class DocAL[A](doc: String, value: Dsl[A]) extends AL[A]
  case class EnsureAL[A](e: A => Boolean, r: A => String, value: Dsl[A]) extends AL[A]
  // case class OptionalAL[A](value: Dsl[A]) extends AL[A]
}

object Dsl {
  import Algebra._
  import FreeInvariantMonoidal.lift
  type Dsl[A] = FreeInvariantMonoidal[AL, A]
  
  def int(field: String): Dsl[Int] = lift(IntAL(field, identity, identity))
  def str(field: String): Dsl[String] = lift(StrAL(field, identity, identity))
  def obj[A](field: String)(value: Dsl[A]): Dsl[A] = lift(ObjAL(field, value))
  
  def doc[A](field: String)(value: Dsl[A]): Dsl[A] = lift(DocAL(field, value))
  def ensure[A](e: A => Boolean, r: A => String)(value: Dsl[A]): Dsl[A] = lift(EnsureAL(e, r, value))
  // def optional[A](value: Dsl[A]): Dsl[Option[A]] = lift(OptionalAL(value))
}

object Helpers {
  import Dsl._
  
  def getField[A](v: Dsl[_]) = "Can probably be obtained by recursively foldind over `v`..."
  
  def min(i: Int)(value: Dsl[Int]): Dsl[Int] = {
    def msg(j: Int) = s"Minimum authorised value for field ${getField(value)} is $i, given $j"
    ensure[Int](i.>=, msg)(value)
  }
  
  def max(i: Int)(value: Dsl[Int]): Dsl[Int] = {
    def msg(j: Int) = s"Maximum authorised value for field ${getField(value)} is $i, given $j"
    ensure[Int](i.<=, msg)(value)
  }
  
  def nonEmpty(value: Dsl[String]): Dsl[String] = {
    def msg(s: String) = s"Empty string for field ${getField(value)})"
    ensure[String](_.nonEmpty, msg)(value)
  }
  
  def increasing[A](value: Dsl[(A, A)])(implicit o: Ordering[A]): Dsl[(A, A)] = {
    def msg(p: (A, A)) = s"$p is not increating"
    ensure[(A, A)](Function.tupled(o.lt), msg)(value)
  }
  
  // The `???` and the `.right.get` are safe if the natural transformations handle `ensure` as expected.
  def validate[A, B](f: A => Either[String, B], g: B => A)(value: Dsl[A]): Dsl[B] =
    (value % ensure[A](a => f(a).isLeft, a => f(a).fold(identity, _ => "???")))
      .imap(a => f(a).right.get)(g)
}
