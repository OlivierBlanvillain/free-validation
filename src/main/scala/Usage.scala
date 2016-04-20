package free.validation

import cats.syntax.cartesian._
import free.validation.Dsl.CoreDsl
import play.api.libs.json._
import scala.Function.unlift

object Usage extends App {
  // type JsonAL[A] = Coproduct[NamedAL, JsonExtraAL, A]

  import CoreDsl.playStyle._

  case class Pet(name: String, weight: Int)
  case class Person(name: String, age: Int, pet: Option[Pet])

  implicit val petConfig =
    (
      (__ \ "name").as[String] |@|
      (__ \ "weight").as[Int]
    ).imap(Pet.apply)(unlift(Pet.unapply))

  val personConfig =
    (
      (__ \ "name").as[String] |@|
      (__ \ "age").as[Int] |@|
      (__ \ "pet").as[Option[Pet]]
    ).imap(Person.apply)(unlift(Person.unapply))

  val codec: Codec[Person, JsObject] =
    personConfig.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)

  val me = Person("Olivier", 25, Some(Pet("sansan", 10)))
  val me2 = Person("Olivier", 25, None)

  val json: JsObject = codec.writes(me)
  val json2: JsObject = codec.writes(me2)
  val validated = codec.validate(json)
  val validated2 = codec.validate(json2)

  println(json)
  println(json2)
  println(validated)
  println(validated2)
}
