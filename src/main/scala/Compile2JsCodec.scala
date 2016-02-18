package free.validation

import Algebra._
import cats.arrow.NaturalTransformation
import play.api.libs.json._

object Compile2JsCodec {
  def nt: NaturalTransformation[AL, Codec[?, JsObject]] =
    new NaturalTransformation[AL, Codec[?, JsObject]] {
      import play.api.data.mapping._
      import play.api.data.mapping.json.Rules._
      import play.api.data.mapping.json.Writes._
      
      def apply[A](value: AL[A]): Codec[A, JsObject] = {
        value match {
          case IntAL(field, value, cvalue) =>
            new Codec[A, JsObject] {
              def validate(data: JsObject): VA[A] =
                (Path \ field).read[JsValue, Int].validate(data).map(value)
              def writes(i: A): JsObject =
                (Path \ field).write[Int, JsValue].writes(cvalue(i)).as[JsObject]
            }
          
          case StrAL(field, value, cvalue) =>
            new Codec[A, JsObject] {
              def validate(data: JsObject): VA[A] =
                (Path \ field).read[JsValue, String].validate(data).map(value)
              def writes(i: A): JsObject =
                (Path \ field).write[String, JsValue].writes(cvalue(i)).as[JsObject]
            }
          
          case ObjAL(field, value) =>
            implicit val c = value.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)
            new Codec[A, JsObject] {
              def validate(data: JsObject): VA[A] =
                (Path \ field).read[JsObject, A].validate(data)
              def writes(i: A): JsObject =
                (Path \ field).write[A, JsObject].writes(i)
            }
          
          case DocAL(field, value) =>
            value.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)
          
          case EnsureAL(e, error, value) =>
            val c = value.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)
            new Codec[A, JsObject] {
              def validate(data: JsObject): VA[A] =
              c.validate(data).fold(Failure.apply, a =>
                if(e(a)) Success(a) else Failure(Seq((Path, Seq(ValidationError(error(a)))))))
              def writes(i: A): JsObject = c.writes(i)
            }
        }
      }
    }
}
    
