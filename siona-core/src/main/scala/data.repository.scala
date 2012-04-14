//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import mapping._
import model._
import query._

package object repository {
  implicit def field2input[T](f: Field[T]) = f(_: model.Input)
  implicit def indexed2predictable[T](field: Indexed[T]) = query.Predictable[T, Indexed](field)
}

package repository {
  trait Repository[M <: Model] {
    type Key

    val model: M

    implicit def field2ops[T](f: Field[T]) = FieldOps(f)

    case class FieldOps[T](field: Field[T]) {
      def get(key: Key) = Repository.this.get(key, field)
      def set(key: Key, x: T) = Repository.this.set(key, field)(x)
      def update(key: Key, x: T => T) = Repository.this.update(key, field)(x)
    }

    def get[T](k: Key, x: M#In => T) = None.asInstanceOf[Operation[T]]
    def get[T](k: Key, f: Field[T]) = None.asInstanceOf[Operation[T]]
    def get[T0, T1](k: Key)(fs: (Field[T0], Field[T1])) = None.asInstanceOf[Operation[(T0, T1)]]

    def set[T](k: Key, x: M#Out => T) = None.asInstanceOf[Operation[T]]
    def set[T](k: Key, f: Field[T])(x: T) = None.asInstanceOf[Operation[T]]
    def set[T0, T1](k: Key)(fs: (Field[T0], Field[T1]))(xs: (T0, T1)) = None.asInstanceOf[Operation[(T0, T1)]]

    def update[T](k: Key, i: M#In => T)(o: T=> M#Out => T) =
      get(k, i).flatMap(x => set(k, o(x)))
    def update[T](k: Key, f: Field[T])(x: T => T) =
      get(k, f).flatMap(set(k, f))
    def update[T0, T1](k: Key)(fs: (Field[T0], Field[T1]))(ts: (T0, T1) => (T0, T1)) = 
      get(k)(fs).flatMap(x => set(k)(fs)(ts(x._1, x._2)))

    def query[T](xs: Predicate[_, Indexed])(x: M#In => T) = None.asInstanceOf[Operation[List[T]]]
    def query[T](xs: Predicate[_, Indexed], f: Field[T]) = None.asInstanceOf[Operation[T]]
    def query[T0, T1](xs: Predicate[_, Indexed], fs: (Field[T0], Field[T1])) = None.asInstanceOf[Operation[(T0, T1)]]
  }


  object Operation {
    def apply[T](x: T) = new Operation[T] { override lazy val value = x }
    //def bind[A](op: Operation[A])(
  }

  trait Operation[V] {
    def value: V
    def map[T](f: V => T): T = f(value)
    def flatMap[T](f: V => Operation[T]) = Operation(f(value))
  }
}
