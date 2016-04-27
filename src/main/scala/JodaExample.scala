package free.validation

import cats.syntax.cartesian._
import free.validation.Algebra.{DefaultMarks, JsonLikeAlgebra}
import free.validation.Dsl.JsonLikeDsl
import org.joda.time.{DateTime, DateTimeZone}
import cats.free.{FreeApplicative => FreeIM}

object JodaExample {
  val coreDsl: JsonLikeDsl[DefaultMarks] = implicitly; import coreDsl._
  
  type AL[T] = JsonLikeAlgebra[DefaultMarks, T]

  val freeDateTime: FreeIM[AL, DateTime] = (
    (
      (__ \ "unix").as[Long]() |@|
      (__ \ "zone").as[String]().map(DateTimeZone.forID)
    ).map { case (unix, zone) => new DateTime(unix, zone) }
  )
}
