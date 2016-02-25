package free.validation

import cats.syntax.cartesian._
import play.api.libs.json._
import scala.Function.{unlift, tupled}
import free.validation.Dsl._
import free.validation.Helpers._

object Usage extends App {
  case class Pet(name: String, weight: Int)
  case class OS(name: Option[String], weight: Seq[Int])
  case class Person(name: String, age: Int, pet: Pet)
  case class Range(start: Int, end: Int)
  
  (string("name") % optional |@| seq(int("weight")))
    .imap(OS.apply)(unlift(OS.unapply))
      
  // Simple validation:
  val petConfig =
    (string("name") |@| int("weight"))
      .imap(Pet.apply)(unlift(Pet.unapply))
  
  // Fields with validation:
  (string("name") % nonEmpty |@| int("weight") % min(10) % max(100))
    .imap(Pet.apply)(unlift(Pet.unapply))

  // Fields with doc:
  ( string("name") |@|
    int("age") |@|
    petConfig % obj("pet")
  ).imap(Person.apply)(unlift(Person.unapply))
  
  // Validating two fields at once:
  increasing((int("start") |@| int("end")).tupled)
    .imap(tupled(Range.apply))(unlift(Range.unapply))
  
  val personConfig =
    (string("name") |@| int("age") |@| obj("pet")(petConfig))
      .imap(Person.apply)(unlift(Person.unapply))
  
  val codec: Codec[Person, JsObject] =
    personConfig.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)
  
  val me = Person("Olivier", 25, Pet("sansan", 10))
  
  val json: JsObject = codec.writes(me)
  val validated = codec.validate(json)
  
  println(json)
  println(validated)
}
