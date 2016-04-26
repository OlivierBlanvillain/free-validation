package free.validation

import cats.syntax.cartesian._
import free.validation.Algebra.{DefaultMarks, JsonLikeAlgebra}
import free.validation.Dsl.JsonLikeDsl
import org.joda.time.{DateTime, DateTimeZone}

object JodaExample {
  val coreDsl: JsonLikeDsl[DefaultMarks] = implicitly; import coreDsl._
  
  type AL[T] = JsonLikeAlgebra[DefaultMarks, T]

  val freeDateTime: FreeIM[AL, DateTime] = (
    (
      (__ \ "unix").as[Long]() |@|
      (__ \ "zone").as[String]().imap(DateTimeZone.forID)(_.getID)
    ).imap
      { case (unix, zone) => new DateTime(unix, zone) }
      { case dateTime => (dateTime.getMillis() / 1000, dateTime.getZone) }
  )
}
