package free.validation

import cats.free.{FreeApplicative => FreeIM}
import cats.syntax.cartesian._
import free.validation.Algebra.{DefaultMarks, JsonLikeAlgebra}
import free.validation.Dsl.{JsonLikeDsl}
import play.api.data.mapping.RuleLike
import play.api.libs.json.JsObject
import play.api.libs.json.Json

object UsageApplicative extends App {
  val coreDsl: JsonLikeDsl[DefaultMarks] = implicitly; import coreDsl._
  
  type AL[T] = JsonLikeAlgebra[DefaultMarks, T]

  case class Pet(name: String, weight: Int)
  case class Person(name: String, age: Int, pet: Option[Pet])
  
  implicit val petConfig: FreeIM[AL, Pet] =
    (
      (__ \ "name").as[String]() |@|
      (__ \ "weight").as[Int]()
    ).map(Pet.apply)
  
  val personConfig: FreeIM[AL, Person] =
    (
      (__ \ "name").as[String](nonEmpty) |@|
      (__ \ "age").as[Int]() |@|
      (__ \ "pet").as[Option[Pet]]()
    ).map(Person.apply)
  
  val rule: RuleLike[JsObject, Person] =
    personConfig.foldMap[Codec[?, JsObject]](Compile2JsCodec.default)
  
  val me = Person("Olivier", 25, Some(Pet("sansan", 10)))
  val me2 = Person("Olivier", 25, None)
  val me3 = Person("", 25, Some(Pet("sansan", 10)))

  val json = Json.parse("""{"name":"Olivier","age":25,"pet":{"name":"sansan","weight":10}}""").as[JsObject]
  val json2 = Json.parse("""{"name":"Olivier","age":25,"pet":null}""").as[JsObject]
  val json3 = Json.parse("""{"name":"","age":25,"pet":{"name":"sansan","weight":10}}""").as[JsObject]

  val validated = rule.validate(json)
  val validated2 = rule.validate(json2)
  val validated3 = rule.validate(json3)
  
  assert(validated.toString ==
    """Success(Person(Olivier,25,Some(Pet(sansan,10))))""")

  assert(validated2.toString ==
    """Success(Person(Olivier,25,None))""")

  assert(validated3.toString ==
    """Failure(List((/name,ArrayBuffer(ValidationError(List(empty string),WrappedArray())))))""")
  
  println(validated)
  println(validated2)
  println(validated3)
  
  println
  
  // CustomUsage.run
}
