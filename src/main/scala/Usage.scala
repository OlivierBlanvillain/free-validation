package free.validation

import cats.syntax.cartesian._
import play.api.libs.json._
import scala.Function.{unlift, tupled}
import free.validation.Dsl._
import free.validation.Helpers._

object Usage extends App {
  case class Pet(name: String, weight: Int)
  case class Person(name: String, age: Int, pet: Pet)
  case class Range(start: Int, end: Int)
  
  // Simple validation:
  val petConfig =
    (str("name") |@| int("weight"))
      .imap(Pet.apply)(unlift(Pet.unapply))
  
  // Fields with validation:
  (str("name") % nonEmpty |@| int("weight") % min(10) % max(100))
    .imap(Pet.apply)(unlift(Pet.unapply))

  // Fields with doc:
  ( str("name") % doc("This is the full name (first name + last name) of a Person") |@|
    int("age") % doc("This is the age of a Person, in days / 365") |@|
    petConfig % obj("pet") % doc("That's his pet.")
  ).imap(Person.apply)(unlift(Person.unapply))
  
  // Validating two fields at once:
  increasing((int("start") |@| int("end")).tupled)
    .imap(tupled(Range.apply))(unlift(Range.unapply))
  
  val personConfig =
    (str("name") |@| int("age") |@| obj("pet")(petConfig))
      .imap(Person.apply)(unlift(Person.unapply))
  
  val codec: Codec[Person, JsObject] =
    personConfig.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)
  
  val me = Person("Olivier", 25, Pet("sansan", 10))
  
  val json: JsObject = codec.writes(me)
  val validated = codec.validate(json)
  
  println(json)
  println(validated)
}
