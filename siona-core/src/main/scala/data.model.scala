//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import scalaz._

import mapping._

package object model {
  type Validation[T] = T => ValidationNEL[String, T]
}

package model {
  trait Input {
    def apply[T](f: Field[T]) = f.apply(this)
    def read[T](n: String): Option[T]
  }
  trait Output {
    def write[T](n: String, x: T): T
  }

  trait Model {
    type F[T] <: model.Field[T]
    type In <: Input
    type Out <: Output
  }

  trait Document extends Model {
    override type F[T] = Indexed[T] with Validated[T]

    abstract class Field[T](override val name: String)(implicit m: Monoid[T])
        extends Default[T] with Indexed[T] with Validated[T] {
      val default = m.zero
    }
  }

  trait Field[T] {
    type V 
    type Z[T] = Field[T]
    val name: String
    def apply(implicit in: Input): V
    def apply(x: T)(out: Output) = out.write(name, x)
  }

  trait Optional[T] extends Field[T] {
    override type V = Option[T]
    def apply(implicit in: Input): V = in.read[T](name)
  }

  trait Default[T] extends Field[T] {
    override type V = T
    val default: T
    def apply(implicit in: Input): V = in.read[T](name).getOrElse(default)
  }

  trait Indexed[T] extends Field[T]

  trait Validated[T] extends Field[T] {
    val validation: model.Validation[T]
    object Validation {
      def apply(v: model.Validation[T]) = v
    }
  }
}
