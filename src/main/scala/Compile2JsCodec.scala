package free.validation

import free.validation.Algebra._
import cats.arrow.NaturalTransformation
import play.api.libs.json._

object Compile2JsCodec {
  def nt: NaturalTransformation[CoreAlgebra, Codec[?, JsObject]] =
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
            implicit val c: Codec[A, JsObject] = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)
            handleLeaf(path)

          // case EnsureAL(e, error, v) =>
            // val c = v.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)

          //   new Codec[A, JsObject] {
          //     def validate(data: JsObject): VA[A] =
          //       c.validate(data).fold(Failure.apply, a =>
          //         if (e(a)) Success(a) else Failure(Seq((Path, Seq(ValidationError(error(a)))))))

          //     def writes(i: A): JsObject = c.writes(i)
          //   }

          case OptionalAL(path, v) =>
            // We know that `type A = Option[B]` for some `B`, but the compiler does not...
            v match {
              case FreeIM.Imap(n, _, _) =>
              val c: Codec[Any, JsObject] = n.foldMap[Codec[?, JsObject]](Compile2JsCodec.nt)

              new Codec[A, JsObject] {
                def validate(data: JsObject): VA[A] = (
                  path.read[JsObject, JsValue].validate(data) match {
                    case Failure(_) | Success(JsNull) => Success(None)
                    case Success(v) => c.validate(v.as[JsObject]).map(Some.apply)
                  }
                ).asInstanceOf[VA[A]]

                def writes(a: A): JsObject =
                  if (a == None) {
                    path.write[JsValue, JsObject].writes(JsNull)
                  } else {
                    val x = a.asInstanceOf[Option[Any]].get
                    path.write(c).writes(x).as[JsObject]
                  }
              }

              case _ =>
                throw new Exception("OptionalAL")
            }

          case SeqAl(path, v) =>
            v match {
              case FreeIM.Imap(n, _, _) => null
              case _ =>
                throw new Exception("SeqAl")
            }
            // ???
        }
      }
    }
}

