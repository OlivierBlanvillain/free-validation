package free.validation

import Algebra._
import cats.arrow.NaturalTransformation
import play.api.libs.json._

object Compile2JsCodec {
  def nt: NaturalTransformation[AL, Codec[?, JsObject]] = nt(false)
  
  def nt(optional: Boolean): NaturalTransformation[AL, Codec[?, JsObject]] =
    new NaturalTransformation[AL, Codec[?, JsObject]] {
      import play.api.data.mapping._
      import play.api.data.mapping.json.Rules._
      import play.api.data.mapping.json.Writes._
      
      private def handleLeaf[A](field: String)
        (implicit r: RuleLike[JsValue, A], w: WriteLike[A, JsValue]): Codec[A, JsObject] =
          new Codec[A, JsObject] {
            def validate(data: JsObject): VA[A] =
              (Path \ field).read[JsValue, A].validate(data)
            def writes(a: A): JsObject =
              (Path \ field).write[A, JsValue].writes(a).as[JsObject]
          }
      
      def apply[A](value: AL[A]): Codec[A, JsObject] = {
        value match {
          case IntAL       (field) => handleLeaf(field)
          case StringAL    (field) => handleLeaf(field)
          case BooleanAL   (field) => handleLeaf(field)
          case ShortAL     (field) => handleLeaf(field)
          case LongAL      (field) => handleLeaf(field)
          case FloatAL     (field) => handleLeaf(field)
          case BigDecimalAL(field) => handleLeaf(field)
          case DoubleAL    (field) => handleLeaf(field)
          
          case ObjectAL(field, v) =>
            implicit val c: Codec[A, JsObject] = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(false))
            handleLeaf(field)
          
          case EnsureAL(e, error, v) =>
            val c = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(optional))
            new Codec[A, JsObject] {
              def validate(data: JsObject): VA[A] =
              c.validate(data).fold(Failure.apply, a =>
                if(e(a)) Success(a) else Failure(Seq((Path, Seq(ValidationError(error(a)))))))
              def writes(i: A): JsObject = c.writes(i)
            }
          
          case OptionalAL(v) =>
            v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(true))
          
          case SeqAl(v) =>
            // AL[Seq[A]]
            implicit val c = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(false))
            ???
        }
      }
    }
}
    
