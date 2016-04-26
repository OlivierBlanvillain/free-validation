package free.validation

import cats.syntax.cartesian._
import free.validation.Algebra.{DefaultMarks, JsonLikeAlgebra}
import free.validation.Dsl.{JsonLikeDsl}
import play.api.libs.json.JsObject
import scala.Function.unlift

object Usage extends App {
  val coreDsl: JsonLikeDsl[DefaultMarks] = implicitly; import coreDsl._
  
  type AL[T] = JsonLikeAlgebra[DefaultMarks, T]

  case class Pet(name: String, weight: Int)
  case class Person(name: String, age: Int, pet: Option[Pet])
  
  implicit val petConfig: FreeIM[AL, Pet] =
    (
      (__ \ "name").as[String]() |@|
      (__ \ "weight").as[Int]()
    ).imap(Pet.apply)(unlift(Pet.unapply))
  
  val personConfig: FreeIM[AL, Person] =
    (
      (__ \ "name").as[String](nonEmpty) |@|
      (__ \ "age").as[Int]() |@|
      (__ \ "pet").as[Option[Pet]]()
    ).imap(Person.apply)(unlift(Person.unapply))

  val codec: Codec[Person, JsObject] =
    personConfig.foldMap[Codec[?, JsObject]](Compile2JsCodec.default)

  val me = Person("Olivier", 25, Some(Pet("sansan", 10)))
  val me2 = Person("Olivier", 25, None)
  val me3 = Person("", 25, Some(Pet("sansan", 10)))

  val json: JsObject = codec.writes(me)
  val json2: JsObject = codec.writes(me2)
  val json3: JsObject = codec.writes(me3)
  
  val validated = codec.validate(json)
  val validated2 = codec.validate(json2)
  val validated3 = codec.validate(json3)

  println(json)
  println(json2)
  println(json3)
  println(validated)
  println(validated2)
  println(validated3)
  
  println
  
  CustomUsage.run
}
