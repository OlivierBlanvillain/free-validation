package free.validation

import free.validation.Algebra._
import cats.arrow.{NaturalTransformation => ~>}
import cats.data.Const
import play.api.libs.json._
import play.api.data.mapping.Path

object Compile2JsonSchema {
  def defaultMarks: DefaultMarks ~> Const[Option[String], ?] =
    new ~>[DefaultMarks, Const[Option[String], ?]] {
      def apply[A](m: DefaultMarks[A]): Const[Option[String], A] = Const(None)
    }
  
  def compile[M[_], A](handleMark: M ~> Const[Option[String], ?])(jla: FreeIM[JsonLikeAlgebra[M, ?], A]): JsObject = {
    val folded = jla.foldMap[Const[JsObject, ?]](nt(handleMark)).getConst
    Json.obj(
      "schema" -> Json.obj(
        "type" -> "object",
        "properties" -> folded
      )
    )
  }
  
  def nt[M[_]](handleMark: M ~> Const[Option[String], ?]): JsonLikeAlgebra[M, ?] ~> Const[JsObject, ?] =
    new ~>[JsonLikeAlgebra[M, ?], Const[JsObject, ?]] {
      private def w(path: Path, tpe: String, doc: Option[String]): JsObject = {
        val theType: JsObject = Json.obj("type" -> tpe)
        val description: JsObject = doc.fold(Json.obj())(d => Json.obj("description" -> d))
        Json.obj(path.path.last.toString -> (theType ++ description))
      }
      
      private def handleMarks[A](marks: Seq[M[A]]): Option[String] =
        marks.map(handleMark.apply).collect { case Const(Some(s)) => s }.headOption
      
      def apply[A](value: JsonLikeAlgebra[M, A]): Const[JsObject, A] = {
        value match {
          case IntAL       (path, marks) => Const(w(path, "number", handleMarks(marks)))
          case StringAL    (path, marks) => Const(w(path, "string", handleMarks(marks)))
          case BooleanAL   (path, marks) => Const(w(path, "boolean", handleMarks(marks)))
          case ShortAL     (path, marks) => Const(w(path, "integer", handleMarks(marks)))
          case LongAL      (path, marks) => Const(w(path, "integer", handleMarks(marks)))
          case FloatAL     (path, marks) => Const(w(path, "integer", handleMarks(marks)))
          case BigDecimalAL(path, marks) => Const(w(path, "number", handleMarks(marks)))
          case DoubleAL    (path, marks) => Const(w(path, "number", handleMarks(marks)))

          case ObjectAL(path, v, marks) =>
            Const(w(path, "object", handleMarks(marks)).deepMerge(
              Json.obj("properties" -> v.foldMap[Const[JsObject, ?]](this).getConst)
            ))

          case OptionalAL(path, v) =>
            val folded = v.foldMap[Const[JsObject, ?]](this).asInstanceOf[Const[JsObject, A]].getConst
            Const(
              Json.obj(
                path.path.last.toString -> Json.obj(
                  "type" -> "object",
                  "properties" -> folded
                )
              )
            )

          case SeqAl(path, v) => Const(Json.obj())
        }
      }
    }
}
