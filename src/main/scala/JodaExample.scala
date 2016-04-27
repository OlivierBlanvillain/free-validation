package free.validation

import cats.syntax.cartesian._
import free.validation.Algebra.{DefaultMarks, JsonLikeAlgebra}
import free.validation.Dsl.JsonLikeDsl
import org.joda.time.{DateTime, DateTimeZone}
import play.api.data.mapping.{Success, Failure}
import scala.language.postfixOps

object JodaExample {
  val coreDsl: JsonLikeDsl[DefaultMarks] = implicitly; import coreDsl._
  
  type AL[T] = JsonLikeAlgebra[DefaultMarks, T]

  val freeDateTimeUnsafe: FreeIM[AL, DateTime] = (
    (
      (__ \ "unix").as[Long]() |@|
      (__ \ "zone").as[String]().imap(DateTimeZone.forID)(_.getID)
    ).imap
      { case (unix, zone) => new DateTime(unix, zone) }
      { case dateTime => (dateTime.getMillis() / 1000, dateTime.getZone) }
  )

  val freeDateTimeTry: FreeIM[AL, DateTime] = (
    imapTry(
      (__ \ "unix").as[Long]() |@|
      (__ \ "zone").as[String]().imap(DateTimeZone.forID)(_.getID) tupled
    ) { case (unix, zone) => new DateTime(unix, zone) }
      { case dateTime => (dateTime.getMillis() / 1000, dateTime.getZone) }
  )
  
  val freeDateTimeVA: FreeIM[AL, DateTime] = (
    imapVA(
      (__ \ "unix").as[Long]() |@|
      (__ \ "zone").as[String]().imap(DateTimeZone.forID)(_.getID) tupled
    ) { case (unix, zone) => if (unix > 0) Success(new DateTime(unix, zone)) else Failure(null) }
      { case dateTime => (dateTime.getMillis() / 1000, dateTime.getZone) }
  )
}
