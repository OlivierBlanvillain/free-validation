/*

package free.validation

import cats.arrow.{NaturalTransformation => ~>}
import cats.data.{Coproduct, Const, Kleisli}
import cats.free.Inject
import cats.syntax.cartesian._
import free.validation.Algebra.{DefaultMarks, JsonLikeAlgebra}
import free.validation.Dsl.JsonLikeDsl
import play.api.libs.json._
import scala.Function.unlift

object Doc {
  case class DocMark[A](documentation: String)
  
  class DocDsl[M[_]](implicit I: Inject[DocMark, M]) {
    def doc[A](documentation: String): M[A] = I.inj(DocMark(documentation))
  }
  
  object DocDsl {
    implicit def injectDocDsl[N[_]](implicit I: Inject[DocMark, N]): DocDsl[N] = new DocDsl[N]
  }
  
  val nt = new ~>[DocMark, Kleisli[Option, ?, String]] {
    def apply[A](mark: DocMark[A]): Kleisli[Option, A, String] = Kleisli(_ => None)
  }
}

object CustomUsage {
  def run(): Unit = {
    import Doc._
    
    type MyMarks[A] = Coproduct[DocMark, DefaultMarks, A]
    type AL[T] = JsonLikeAlgebra[MyMarks, T]
    
    val coreDsl: JsonLikeDsl[MyMarks] = implicitly; import coreDsl._
    val docDsl: DocDsl[MyMarks]   = implicitly; import docDsl._

    case class Pet(name: String, weight: Int)
    case class Person(name: String, age: Int, pet: Option[Pet])
    
    implicit val petConfig: FreeIM[AL, Pet] =
      (
        (__ \ "name").as[String]() |@|
        (__ \ "weight").as[Int]()
      ).imap(Pet.apply)(unlift(Pet.unapply))
    
    val personConfig: FreeIM[AL, Person] =
      (
        (__ \ "name").as[String](nonEmpty, doc("here is my name")) |@|
        (__ \ "age").as[Int](doc("that's the age")) |@|
        (__ \ "pet").as[Option[Pet]]()
      ).imap(Person.apply)(unlift(Person.unapply))
    
    val codec: Codec[Person, JsObject] =
      personConfig.foldMap[Codec[?, JsObject]](Compile2JsCodec.compile[MyMarks](Doc.nt or Compile2JsCodec.defaultMarks))

    val me = Person("Olivier", 25, Some(Pet("sansan", 10)))
    val me2 = Person("Olivier", 25, None)
    val me3 = Person("", 25, Some(Pet("sansan", 10)))

    val json: JsObject = codec.writes(me)
    val json2: JsObject = codec.writes(me2)
    val json3: JsObject = codec.writes(me3)
    
    val validated = codec.validate(json)
    val validated2 = codec.validate(json2)
    val validated3 = codec.validate(json3)

    assert(json.toString ==
      """{"name":"Olivier","age":25,"pet":{"name":"sansan","weight":10}}""")

    assert(json2.toString ==
      """{"name":"Olivier","age":25,"pet":null}""")

    assert(json3.toString ==
      """{"name":"","age":25,"pet":{"name":"sansan","weight":10}}""")

    assert(validated.toString ==
      """Success(Person(Olivier,25,Some(Pet(sansan,10))))""")

    assert(validated2.toString ==
      """Success(Person(Olivier,25,None))""")

    assert(validated3.toString ==
      """Failure(List((/name,ArrayBuffer(ValidationError(List(empty string),WrappedArray())))))""")

    val docNT: DocMark ~> Const[Option[String], ?] =
      new ~>[DocMark, Const[Option[String], ?]] {
        def apply[A](m: DocMark[A]): Const[Option[String], A] = Const(Some(m.documentation))
      }
    
    def myMarksCompiler[A] = Compile2JsonSchema.compile[MyMarks, A](docNT or Compile2JsonSchema.defaultMarks) _
    val jsonSchema: JsObject = myMarksCompiler(personConfig)
    
    assert(Json.prettyPrint(jsonSchema) ==
      """{
      |  "schema" : {
      |    "type" : "object",
      |    "properties" : {
      |      "name" : {
      |        "type" : "string",
      |        "description" : "here is my name"
      |      },
      |      "age" : {
      |        "type" : "number",
      |        "description" : "that's the age"
      |      },
      |      "pet" : {
      |        "type" : "object",
      |        "properties" : {
      |          "name" : {
      |            "type" : "string"
      |          },
      |          "weight" : {
      |            "type" : "number"
      |          }
      |        }
      |      }
      |    }
      |  }
      |}""".stripMargin
    )
  }
}

*/
