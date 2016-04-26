package free

import cats.Monoid
import play.api.libs.json._

package object validation {
  implicit def jsObjectMonoid = new Monoid[JsObject] {
    def empty: JsObject = Json.obj()
    def combine(a1: JsObject, a2: JsObject): JsObject = a1 deepMerge a2
  }
}
