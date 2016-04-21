package free.validation

import free.validation.Algebra._
import cats.arrow.{NaturalTransformation => ~>}
import cats.data.Kleisli
import play.api.libs.json._
import play.api.data.mapping._

object Compile2JsCodec {
  def default: CoreAlgebra[DefaultMarks, ?] ~> Codec[?, JsObject] =
    compile(defaultMarks)
    
  def defaultMarks: DefaultMarks ~> Kleisli[Option, ?, String] =
    new ~>[DefaultMarks, Kleisli[Option, ?, String]] {
      def apply[A](mark: DefaultMarks[A]): Kleisli[Option, A, String] =
        mark match {
          case NonEmptyM() => Kleisli {
            case a: String if a.isEmpty => Some("empty string")
            case _ => None
          }
          case m @ MinM(v) => Kleisli { a =>
            if (m.n.lt(m.n.fromInt(v), a)) None
            else Some(s"min($v) violated by $a")
          }            
          case m @ MaxM(v) => Kleisli { a =>
            if (m.n.gt(m.n.fromInt(v), a)) None
            else Some(s"max($v) violated by $a")
          }
        }
    }
  
  def compile[M[_]](marksHandler: M ~> Kleisli[Option, ?, String]): CoreAlgebra[M, ?] ~> Codec[?, JsObject] =
    new ~>[CoreAlgebra[M, ?], Codec[?, JsObject]] {
      import play.api.data.mapping.json.Rules._
      import play.api.data.mapping.json.Writes._

      private def handleLeaf[A](path: Path, marks: Seq[M[A]])
        (implicit r: RuleLike[JsValue, A], w: WriteLike[A, JsValue]): Codec[A, JsObject] =
          new Codec[A, JsObject] {
            def validate(data: JsObject): VA[A] = {
              path.read[JsValue, A].validate(data) match {
                case Success(a) =>
                  val errors = marks
                    .map(m => marksHandler(m).run(a))
                    .collect { case Some(e) => ValidationError(e) }
                  
                  errors.headOption match {
                    case None => Success(a)
                    case Some(_) => Failure(Seq(path -> errors))
                  }
                
                case e => e
              }
            }
            def writes(a: A): JsObject =
              path.write[A, JsValue].writes(a).as[JsObject]
          }

      def apply[A](value: CoreAlgebra[M, A]): Codec[A, JsObject] = {
        value match {
          case IntAL       (path, marks) => handleLeaf(path, marks)
          case StringAL    (path, marks) => handleLeaf(path, marks)
          case BooleanAL   (path, marks) => handleLeaf(path, marks)
          case ShortAL     (path, marks) => handleLeaf(path, marks)
          case LongAL      (path, marks) => handleLeaf(path, marks)
          case FloatAL     (path, marks) => handleLeaf(path, marks)
          case BigDecimalAL(path, marks) => handleLeaf(path, marks)
          case DoubleAL    (path, marks) => handleLeaf(path, marks)

          case ObjectAL(path, v, marks) =>
            implicit val c: Codec[A, JsObject] = v.foldMap[Codec[?, JsObject]](this)
            handleLeaf(path, marks)

          case OptionalAL(path, v) =>
            val c = v.foldMap[Codec[?, JsObject]](this)

            new Codec[A, JsObject] {
              def validate(data: JsObject): VA[A] =
                path.read[JsObject, JsValue].validate(data) match {
                  case Failure(_) | Success(JsNull) => Success(None)
                  case Success(v) => c.validate(v.as[JsObject]).map(Some.apply)
                }

              def writes(a: A): JsObject =
                a match {
                  case None => path.write[JsValue, JsObject].writes(JsNull)
                  case Some(x) => path.write(c).writes(x).as[JsObject]
                }
            }
          
          case SeqAl(path, v) =>
            ???
        }
      }
    }
}

