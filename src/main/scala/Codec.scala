package free.validation

import cats._
import play.api.data.mapping._

/** A = Algebric data type (illegal state is unrepresentable)
  * B = Bastard data type  (illegal state is representable) */
trait Codec[A, B] extends RuleLike[B, A] with WriteLike[A, B] { self =>
  def imap[AA](f: A => AA)(g: AA => A): Codec[AA, B] =
    new Codec[AA, B] {
      def validate(b: B): VA[AA] = self.validate(b).map(f)
      def writes(aa: AA): B = self.writes(g(aa))
    }

  def validationProduct[E, T, U](va: Validation[E, T], vb: Validation[E, U]): Validation[E, (T, U)] = {
    val apply =  Validation.applicativeValidation[E].apply[T, (T, U)] _
    apply(vb.map(x => (_ -> x)), va)
  }

  def product[AA](ff: Codec[AA, B])(implicit s: Semigroup[B]): Codec[(A, AA), B] =
    new Codec[(A, AA), B] {
      def validate(b: B): VA[(A, AA)] = validationProduct(self.validate(b), (ff.validate(b)))
      def writes(aaa: (A, AA)): B = s.combine(self.writes(aaa._1), ff.writes(aaa._2))
    }
}

object Codec {
  implicit def codecIsInvariantMonoidal[B](implicit m: Monoid[B]): InvariantMonoidal[Codec[?, B]] =
    new InvariantMonoidal[Codec[?, B]] {
      def pure[A](a: A): Codec[A, B] = new Codec[A, B] {
        def validate(data: B): VA[A] = Success(a)
        def writes(i: A): B = m.empty
      }

      def imap[A, AA](fa: Codec[A, B])(f: A => AA)(g: AA => A): Codec[AA, B] =
        fa.imap(f)(g)

      def product[A, AA](fa: Codec[A, B], fb: Codec[AA, B]): Codec[(A, AA), B] =
        fa.product(fb)
    }
}
