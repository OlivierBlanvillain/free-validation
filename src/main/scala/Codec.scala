package free.validation

import cats.{Monoid, Semigroup, Applicative}
import play.api.data.mapping.{RuleLike, WriteLike, VA, Validation, Success}

/** A = Algebric data type (illegal state is not representable)
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
  /*
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
  */
    
  implicit def codecIsAlmostApplicative[C](implicit m: Monoid[C]): Applicative[Codec[?, C]] =
    new Applicative[Codec[?, C]] {
      def pure[A](a: A): Codec[A, C] = new Codec[A, C] {
        def validate(data: C): VA[A] = Success(a)
        def writes(i: A): C = m.empty
      }
      
      def ap[A, B](ff: Codec[A => B, C])(fa: Codec[A, C]): Codec[B, C] =
        map(ff.product(fa))(x => x._1(x._2))
      
      def product[A, B](fa: Codec[A, C],fb: Codec[B, C]): Codec[(A, B), C] =
        fa.product(fb)
      
      // Close your eyes
      def map[A, B](fa: Codec[A, C])(f: A => B): Codec[B, C] =
        fa.imap(f)(null)
      // Open your eyes
    }
}
