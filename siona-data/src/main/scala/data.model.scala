//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import scalaz._

import scalaz.effect._

import io._

package object model {
  type Validator[T] = T => ValidationNEL[String, T]
}

package model {
  trait Model {
    type F[T] <: model.Field[T]
    //type In <: Input
    //type Out <: Output
  }

  trait Document extends Model {
    override type F[T] = Indexed[T] with Validated[T]

    case class Field[T](
      override val name: String,
      override val validation: Validator[T]
    )(implicit val m: Monoid[T]) extends DocumentField[T]

    protected trait DocumentField[T] extends Default[T] with Indexed[T] with Validated[T] {
      implicit val m: Monoid[T]
      val default = m.zero
    }
  }

  trait Field[T] {
    type V 
    type Z[T] = Field[T]
    val name: String
    def apply(implicit in: Input, s: Serializable[T]): V
    def apply(x: T)(implicit out: Output, s: Serializable[T]): IO[T] = IO(out.write(name, x))
  }

  trait Optional[T] extends Field[T] {
    override type V = Option[T]
    override def apply(implicit in: Input, s: Serializable[T]): V = in.read[T](name)
  }

  trait Default[T] extends Field[T] {
    override type V = T
    val default: T
    override def apply(implicit in: Input, s: Serializable[T]): V = in.read[T](name).getOrElse(default)
  }

  trait Indexed[T] extends Field[T]

  trait Validated[T] extends Field[T] {
    val validation: model.Validator[T]
  }
  object Validator {
    def apply[T](v: model.Validator[T]) = v
  }
}
