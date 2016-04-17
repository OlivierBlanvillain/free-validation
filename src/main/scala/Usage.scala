package free.validation

import cats.syntax.cartesian._
import free.validation.Dsl.CoreDsl
import play.api.libs.json._
import scala.Function.unlift

object Usage extends App {
  // type JsonAL[A] = Coproduct[NamedAL, JsonExtraAL, A]

  import CoreDsl.playStyle._

  case class Pet(name: String, weight: Int)
  case class Person(name: String, age: Int, pet: Pet)

  implicit val petConfig =
    (
      (__ \ "name").as[String] |@|
      (__ \ "weight").as[Int]
    ).imap(Pet.apply)(unlift(Pet.unapply))

  val personConfig =
    (
      (__ \ "name").as[String] |@|
      (__ \ "age").as[Int] |@|
      (__ \ "pet").as[Pet]
    ).imap(Person.apply)(unlift(Person.unapply))

  val codec: Codec[Person, JsObject] =
    personConfig.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)

  val me = Person("Olivier", 25, Pet("sansan", 10))

  val json: JsObject = codec.writes(me)
  val validated = codec.validate(json)

  println(json)
  println(validated)
}
