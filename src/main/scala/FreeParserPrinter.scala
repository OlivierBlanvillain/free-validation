import cats.arrow.NaturalTransformation
import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import cats.syntax.monoidal._

object Algebra {
  sealed trait Format[A]
  case class IntFormat[A](field: String, value: Int => A) extends Format[A]
  case class StrFormat[A](field: String, value: String => A) extends Format[A]
  case class ObjFormat[A](field: String, value: FreeApplicative[Format, A]) extends Format[A]
  
  // sealed trait JValue
  // case object JNull extends JValue
  // case class JBool(value: Boolean) extends JValue
  // case class JNumber(value: String) extends JValue {
  //   def toDouble: Double = value.toDouble
  // }
  // case class JString(value: String) extends JValue
  // case class JArray(value: Seq[JValue]) extends JValue
  // case class JObject(value: Seq[(String, JValue)]) extends JValue
}

object Dsl {
  import Algebra._
  
  type Dsl[A] = FreeApplicative[Format, A]
  
  def int(field: String): Dsl[Int] = lift(IntFormat(field, identity))
  def str(field: String): Dsl[String] = lift(StrFormat(field, identity))
  def obj[A](field: String, value: Dsl[A]): Dsl[A] = lift(ObjFormat(field, value))
}

object FreeParserPrinter {
  import Dsl._
  
  trait Printer[A] {
    def print(a: A): String
  }
  
  def genPrinter[A](format: Dsl[A]): Printer[A] =
    format.foldMap(new NaturalTransformation[Dsl, Printer] {
      def apply[A](value: Dsl[A]): Printer[A] = {
        new Printer[A] {
          def print(a: A): String = {
            ""
          }
        }
      }
    })
}

object Example {
  import Dsl._
  
  case class Birth(year: Int, month: Int, day: Int)
  case class Person(birth: Birth, name: String, age: Int)
  
  val birthFreeAp: Dsl[Birth] =
    (
      int("year") |@|
      int("month") |@|
      int("day")
    ).map(Birth.apply)
  
  val isi: Dsl[Person] =
    (
      obj("birth", birthFreeAp) |@|
      str("name") |@|
      int("age")    
    ).map(Person.apply)
}
