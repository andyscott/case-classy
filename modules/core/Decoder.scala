/* -
 * Case Classy [case-classy-core]
 */

package classy
package core

import scala.util.{ Either, Right }

import wheel._

/** A type class capturing the ability to decode data of type `A` to `B`.
  */
trait Decoder[A, B] extends Serializable {

  /** Decode a value of type `A` as type `B` */
  def decode(a: A): Either[DecodeError, B]

  //

  import Decoder.instance

  /** Construct a new decoder by mapping the output of this decoder
    */
  final def map[C](f: B ⇒ C): Decoder[A, C] =
    instance(input ⇒ decode(input).map(f))

  /** Construct a new decoder by mapping the output of this decoder
    * to either an `DecodeError` or a new result type
    */
  final def emap[C](f: B ⇒ Either[DecodeError, C]): Decoder[A, C] =
    instance(input ⇒ decode(input).flatMap(f))

  /** Construct a new decoder by mapping the error output of this decoder
    * to a new error
    */
  final def leftMap(f: DecodeError ⇒ DecodeError): Decoder[A, B] =
    instance(input ⇒ decode(input).leftMap(f))

  /** Construct a new decoder by mapping the input to this decoder
    */
  final def mapInput[Z](f: Z ⇒ A): Decoder[Z, B] =
    instance(input ⇒ decode(f(input)))

  /** Construct a new decoder through a monadic bind */
  final def flatMap[C](f: B ⇒ Decoder[A, C]): Decoder[A, C] =
    instance(input ⇒ decode(input).flatMap(b ⇒ f(b).decode(input)))

  /** Construct a new decoder by using the result of another decoder as
    * the input to this decoder
    */
  final def compose[Z](previous: Decoder[Z, A]): Decoder[Z, B] =
    instance(input ⇒ previous.decode(input).flatMap(decode))

  /** Construct a new decoder by decoding on the result of this decoder */
  final def andThen[C](next: Decoder[B, C]): Decoder[A, C] =
    instance(input ⇒ decode(input).flatMap(next.decode))

  /** Construct a new decober by joining this decoder with another,
    * tupling the results
    */
  final def and[C](that: Decoder[A, C]): Decoder[A, (B, C)] =
    instance { input ⇒
      val rb = decode(input)
      val rc = that.decode(input)
      (rb, rc) match {
        case (Right(b), Right(c)) ⇒ (b, c).right
        case (Left(eb), Left(ec)) ⇒ (eb && ec).left
        case (Left(eb), _)        ⇒ eb.left
        case (_, Left(ec))        ⇒ ec.left
      }
    }

  /** Construct a new decoder using this decoder. If it fails, use the
    * other.
    */
  final def or[BB >: B](that: Decoder[A, BB]): Decoder[A, BB] =
    instance(input ⇒ decode(input) match {
      case right @ Right(_) ⇒ right
      case _                ⇒ that.decode(input)
    })

  final def option: Decoder[A, Option[B]] =
    instance(input ⇒ decode(input).toOption.right)

  final def either: Decoder[A, Either[DecodeError, B]] =
    instance(input ⇒ Right(decode(input)))

  final def sequence[F[_]](implicit F: Traversable[F]): Decoder[F[A], F[B]] =
    instance(fa ⇒ F.sequence(F.map(fa)(decode)))
}

object Decoder {
  def apply[A, B](implicit ev: Decoder[A, B]): Decoder[A, B] = ev

  /** Construct a new decoder using function `f` for decoding */
  def instance[A, B](f: A ⇒ Either[DecodeError, B]): Decoder[A, B] = new Decoder[A, B] {
    override def decode(a: A): Either[DecodeError, B] = f(a)
  }

  /** Construct a decoder that always decodes a constant value */
  def const[A, B](value: B): Decoder[A, B] = instance(_ ⇒ value.right)

  implicit class TupleOps1[Θ, A](val left: Decoder[Θ, A]) extends AnyVal {
    def ~[B](right: Decoder[Θ, B]): Decoder[Θ, (A, B)] = left and right
  }

  implicit class TupleOps2[Θ, A, B](val left: Decoder[Θ, (A, B)]) extends AnyVal {
    def ~[C](right: Decoder[Θ, C]): Decoder[Θ, (A, B, C)] = (left and right) map {
      case ((a, b), c) ⇒ (a, b, c)
    }
  }

  implicit class TupleOps3[Θ, A, B, C](val left: Decoder[Θ, (A, B, C)]) extends AnyVal {
    def ~[D](right: Decoder[Θ, D]): Decoder[Θ, (A, B, C, D)] = (left and right) map {
      case ((a, b, c), d) ⇒ (a, b, c, d)
    }
  }

  implicit class TupleOps4[Θ, A, B, C, D](val left: Decoder[Θ, (A, B, C, D)]) extends AnyVal {

    def mapN[Ω](f: (A, B, C, D) ⇒ Ω): Decoder[Θ, Ω] = left map {
      case (a, b, c, d) ⇒ f(a, b, c, d)
    }

    def ~[E](right: Decoder[Θ, E]): Decoder[Θ, (A, B, C, D, E)] = (left and right) map {
      case ((a, b, c, d), e) ⇒ (a, b, c, d, e)
    }
  }

}
