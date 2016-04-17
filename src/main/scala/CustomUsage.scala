package free.validation

import cats.data.Coproduct
import cats.free.Inject
import cats.syntax.cartesian._
import free.validation.Algebra.CoreAlgebra
import free.validation.Dsl.{CoreDsl, PlayStyleDsp}
import play.api.data.mapping.Path
import scala.Function.unlift

object CustomUsage {
  sealed trait DocumentationAlgebra[A]
  case class DocAL[A](documentation: String, value: FreeIM[CoreAlgebra, A]) extends DocumentationAlgebra[A]
  case class NullAL[A](field: Path) extends DocumentationAlgebra[Null]

  class DocumentationDsl[F[_]](implicit I: Inject[DocumentationAlgebra, F]) {
    private def lift[A](a: DocumentationAlgebra[A]): FreeIM[F, A] = FreeIM.inject[DocumentationAlgebra, F](a)

    implicit def doc[A](documentation: String)(value: FreeIM[CoreAlgebra, A]): FreeIM[F, A] =
      lift(DocAL(documentation, value))

    implicit def nul(path: Path): FreeIM[F, Null] =
      lift(NullAL(path))
  }

  type MyAL[A] = Coproduct[CoreAlgebra, DocumentationAlgebra, A]

  // Guide the implicit search a bit
  implicit val faa: InvariantMonoidal[FreeIM[MyAL, ?]] = implicitly

  case class Pet(name: String, weight: Int)
  case class Person(name: String, age: Int, pet: Pet)

  object playStyle extends PlayStyleDsp[MyAL]

  def config(implicit A1: CoreDsl[MyAL], A2: DocumentationDsl[MyAL]): FreeIM[MyAL, Person] = {
    import A1._ // , A2._
    import playStyle._

    implicit val petConfig: FreeIM[MyAL, Pet] =
      (
        (__ \ "name").as[String] |@|
        (__ \ "weight").as[Int]
      ).imap(Pet.apply)(unlift(Pet.unapply))

    (
      (__ \ "name").as[String] |@|
      (__ \ "age").as[Int] |@|
      obj(__ \ "pet")(petConfig)
    ).imap(Person.apply)(unlift(Person.unapply))
  }

}
