import cats.arrow.NaturalTransformation
import cats.data.Validated
import cats.functor._
import cats.syntax.cartesian._
import cats._

import play.api.libs.json._
import play.api.data.mapping._

import Function.unlift
import simulacrum.typeclass

// trait RuleLike[B, A] {
//   def read(b: B): VA[A]
// }

// trait WriteLike[A, B] {
//   def write(a: A): B
// }

// A = Algebric data type (illegal state is unrepresentable)
// B = Bastard data type  (illegal state is representable)

trait Codec[A, B] extends RuleLike[B, A] with WriteLike[A, B] { self =>
  def imap[AA](f: A => AA)(g: AA => A): Codec[AA, B] =
    new Codec[AA, B] {
      def validate(b: B): VA[AA] = self.validate(b).map(f)
      def writes(aa: AA): B = self.writes(g(aa))
    }
  
  def validationProduct[E, T, U](va: Validation[E, T], vb: Validation[E, U]): Validation[E, (T, U)] = {
    val apply =  Validation.applicativeValidation[E].apply[T, (T, U)] _
    apply(vb.map(x => (_ -> x)), va)
  }
  
  def product[AA](ff: Codec[AA, B])(implicit s: Semigroup[B]): Codec[(A, AA), B] =
    new Codec[(A, AA), B] {
      def validate(b: B): VA[(A, AA)] = validationProduct(self.validate(b), (ff.validate(b)))
      def writes(aaa: (A, AA)): B = s.combine(self.writes(aaa._1), ff.writes(aaa._2))
    }
}

object Toplevel {
  implicit def formatIsInvariantMonoidal[B](implicit s: Semigroup[B]): InvariantMonoidal[Codec[?, B]] =
    new InvariantMonoidal[Codec[?, B]] {
      def pure[A](a: A): Codec[A, B] = ??? // TODO
      def imap[A, AA](fa: Codec[A, B])(f: A => AA)(g: AA => A): Codec[AA, B] = fa.imap(f)(g)
      def product[A, AA](fa: Codec[A, B], fb: Codec[AA, B]): Codec[(A, AA), B] = fa.product(fb)
    }
}; import Toplevel._


object Algebra {
  sealed trait ISO[A]
  case class IntISO[A](field: String, value: Int => A, cvalue: A => Int) extends ISO[A]
  case class StrISO[A](field: String, value: String => A, cvalue: A => String) extends ISO[A]
  case class ObjISO[A](field: String, value: FreeInvariantMonoidal[ISO, A]) extends ISO[A]
}

object Dsl {
  import Algebra._
  import FreeInvariantMonoidal.lift
  type Dsl[A] = FreeInvariantMonoidal[ISO, A]
  
  def int(field: String): Dsl[Int] = lift(IntISO(field, identity, identity))
  def str(field: String): Dsl[String] = lift(StrISO(field, identity, identity))
  def obj[A](field: String, value: Dsl[A]): Dsl[A] = lift(ObjISO(field, value))
}

object JsonLib {
  import Algebra._
  import Dsl._
  import play.api.data.mapping.json.Rules._
  import play.api.data.mapping.json.Writes._
    
  implicit def jsObjectMonoid = new Monoid[JsObject] {
    def combine(a1: JsObject, a2: JsObject): JsObject = a1 deepMerge a2
    def empty: JsObject = Json.obj()
  }
  
  def genCodec[AA](config: Dsl[AA]): Codec[AA, JsObject] =
    config.foldMap[Codec[?, JsObject]](
      new NaturalTransformation[ISO, Codec[?, JsObject]] {
        def apply[A](value: ISO[A]): Codec[A, JsObject] = {
          value match {
            case IntISO(field, value, cvalue) =>
              new Codec[A, JsObject] {
                def validate(data: JsObject): VA[A] =
                  (Path \ field).read[JsValue, Int].validate(data).map(value)
                def writes(i: A): JsObject =
                  (Path \ field).write[Int, JsValue].writes(cvalue(i)).as[JsObject]
              }
            
            case StrISO(field, value, cvalue) =>
              new Codec[A, JsObject] {
                def validate(data: JsObject): VA[A] =
                  (Path \ field).read[JsValue, String].validate(data).map(value)
                def writes(i: A): JsObject =
                  (Path \ field).write[String, JsValue].writes(cvalue(i)).as[JsObject]
              }
            
            case ObjISO(field, value) =>
              implicit val c = genCodec(value)
              new Codec[A, JsObject] {
                def validate(data: JsObject): VA[A] =
                  (Path \ field).read[JsObject, A].validate(data)
                def writes(i: A): JsObject =
                  (Path \ field).write[A, JsObject].writes(i)
              }
          }
        }
      }
    )
}

object Usage extends App {
  import Dsl._
  import JsonLib._
  
  case class Pet(name: String, weight: Int)
  case class Person(name: String, age: Int, pet: Pet)
  
  val petConfig =
    (str("name") |@| int("weight"))
      .imap(Pet.apply)(unlift(Pet.unapply))
  
  val personConfig =
    (str("name") |@| int("age") |@| obj("pet", petConfig))
      .imap(Person.apply)(unlift(Person.unapply))
  
  val codec: Codec[Person, JsObject] = genCodec(personConfig)
  val me = Person("Olivier", 25, Pet("sansan", 10))
  
  val json: JsObject = codec.writes(me)
  val validated: VA[Person] = codec.validate(json)
  
  println(json)
  println(validated)
}
