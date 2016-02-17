import cats.arrow.NaturalTransformation
import cats.data.Validated
import cats.functor._
import cats.syntax.monoidal._
import cats._

import play.api.libs.json._
import play.api.data.mapping._

import Function.unlift
import simulacrum.typeclass

@typeclass trait InvariantMonoidal[F[_]] extends Invariant[F] with Monoidal[F]

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

trait FreeInvariantMonoidal[F[_], A] extends Product with Serializable { self =>
  import FreeInvariantMonoidal.{FA, Zip, Suspend, Imap}
  
  def imap[B](f: A => B)(g: B => A): FA[F, B] =
    Imap(this, f, g)
  
  def product[B](fb: FA[F, B]): FA[F, (A, B)] =
    Zip(this, fb)
  
  def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit im: InvariantMonoidal[G]): G[A] =
    this match {
      case Suspend(fa) => nt(fa)
      case Zip(fa, fb) => im.product(fa.foldMap(nt), fb.foldMap(nt)).asInstanceOf[G[A]]
      case Imap(fa, f, g) => im.imap(fa.foldMap(nt))(f)(g)
    }
}

object FreeInvariantMonoidal {
  type FA[F[_], A] = FreeInvariantMonoidal[F, A]
  
  private final case class Suspend[F[_], A](fa: F[A]) extends FA[F, A]
  
  private final case class Zip[F[_], A, B, C](fa: FA[F, A], fb: FA[F, B]) extends FA[F, C]
  
  private final case class Imap[F[_], A, B](fa: FA[F, A], f: A => B, g: B => A) extends FA[F, B]
  
  def lift[F[_], A](fa: F[A]): FA[F, A] =
    Suspend(fa)
  
  implicit def freeInvariantMonoidal[S[_]]: InvariantMonoidal[FA[S, ?]] =
    new InvariantMonoidal[FA[S, ?]] {
       def imap[A, B](fa: FA[S, A])(f: A => B)(g: B => A): FA[S, B] = fa.imap(f)(g)
       def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] = fa.product(fb)
    }
}
