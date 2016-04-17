package free.validation

import free.validation.Algebra._
import cats.arrow.NaturalTransformation
import play.api.libs.json._

object Compile2JsCodec {
  def nt: NaturalTransformation[CoreAlgebra, Codec[?, JsObject]] = nt(false)

  def nt(optional: Boolean): NaturalTransformation[CoreAlgebra, Codec[?, JsObject]] =
    new NaturalTransformation[CoreAlgebra, Codec[?, JsObject]] {
      import play.api.data.mapping._
      import play.api.data.mapping.json.Rules._
      import play.api.data.mapping.json.Writes._

      private def handleLeaf[A](path: Path)
        (implicit r: RuleLike[JsValue, A], w: WriteLike[A, JsValue]): Codec[A, JsObject] =
          new Codec[A, JsObject] {
            def validate(data: JsObject): VA[A] =
              path.read[JsValue, A].validate(data)
            def writes(a: A): JsObject =
              path.write[A, JsValue].writes(a).as[JsObject]
          }

      def apply[A](value: CoreAlgebra[A]): Codec[A, JsObject] = {
        value match {
          case IntAL       (path) => handleLeaf(path)
          case StringAL    (path) => handleLeaf(path)
          case BooleanAL   (path) => handleLeaf(path)
          case ShortAL     (path) => handleLeaf(path)
          case LongAL      (path) => handleLeaf(path)
          case FloatAL     (path) => handleLeaf(path)
          case BigDecimalAL(path) => handleLeaf(path)
          case DoubleAL    (path) => handleLeaf(path)

          case ObjectAL(path, v) =>
            implicit val c: Codec[A, JsObject] = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(false))
            handleLeaf(path)

          case EnsureAL(e, error, v) =>
            val c = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(optional))

            new Codec[A, JsObject] {
              def validate(data: JsObject): VA[A] =
                c.validate(data).fold(Failure.apply, a =>
                  if (e(a)) Success(a) else Failure(Seq((Path, Seq(ValidationError(error(a)))))))

              def writes(i: A): JsObject = c.writes(i)
            }

          case OptionalAL(v) =>
            v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(true))

          case _ =>
            // CoreAlgebra[Seq[A]]
            // implicit val c = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt(false))
            ???
        }
      }
    }
}

