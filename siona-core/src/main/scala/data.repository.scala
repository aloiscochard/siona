//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import shapeless.{Field => _, _}
import HList._
import Functions._
import TypeOperators._
import CoNatTRel._
import CoNatTRelAux._

import io._
import mapping._
import model._
import query._

package object repository {
  implicit def field2input[T : Serializable](f: Field[T]) = f(_: io.Input, implicitly[Serializable[T]])
  implicit def indexed2predictable[T : Serializable](field: Indexed[T]) = query.Predictable[T, Indexed](field)

  object HList {
    /**
     * Construct an HList from a given Tuple.
     */
    def apply[P <: Product](tuple: P)(implicit hlister: HLister[P]) = hlister(tuple)
  }
}

package repository {
  trait Repository[M <: Model] {
    type Key

    val model: M

    implicit def field2ops[T](f: Field[T]) = FieldOps(f)

    case class FieldOps[T](field: Field[T]) {
      def get(key: Key) = Repository.this.get(key, field)
      def set(key: Key, x: T) = Repository.this.set(key, field, x)
      def update(key: Key, x: T => T) = Repository.this.update(key, field)(x)
    }

    def get[T](k: Key, x: io.Input => T) = None.asInstanceOf[Operation[T]]
    def get[T](k: Key, f: Field[T]) = None.asInstanceOf[Operation[T]]
    def get[L <: HList](k: Key, fs: L)(implicit e: (Field +~??> Id)#λ[L]) = None.asInstanceOf[Operation[e.Out]]


    def set[T](k: Key, x: io.Output => T) = None.asInstanceOf[Operation[T]]
    def set[T](k: Key, f: Field[T], x: T) = None.asInstanceOf[Operation[T]]
    def set[L1 <: HList, L2 <: HList](k: Key, fs: L1, xs: L2)(implicit e: CoNatTRelAux[L1, Field, L2, Id]) =
      None.asInstanceOf[Operation[L2]]


    // Should return just Op[L2] instead of Op[Op[L2]] .. fix flatmap in Operation monad for that
    def update[T](k: Key, i: io.Input => T, o: T=> io.Output => T) =
      get(k, i).flatMap(x => set(k, o(x)))
    def update[T](k: Key, f: Field[T])(x: T => T) =
      get(k, f).flatMap(set(k, f, _))
    def update[L1 <: HList, L2 <: HList](k: Key, fs: L1)(ts: L2 => L2)(implicit e: CoNatTRelAux[L1, Field, L2, Id]) =
       get(k, fs).flatMap(x => set(k, fs, ts(x)))


    def query[T](xs: Predicate[_, Indexed])(x: io.Input => T) = None.asInstanceOf[Operation[List[T]]]
    def query[T](xs: Predicate[_, Indexed], f: Field[T]) = None.asInstanceOf[Operation[T]]
    def query[T0, T1](xs: Predicate[_, Indexed], fs: (Field[T0], Field[T1])) = None.asInstanceOf[Operation[(T0, T1)]]

    // Conversions from tuples
    def get[P <: Product, L <: HList](k: Key)(fs: P)(implicit l: HListerAux[P, L], e: (Field +~??> Id)#λ[L]): Operation[e.Out] =
      get[L](k, l(fs))

    def set[P1 <: Product, P2 <: Product, L1 <: HList, L2 <: HList](k: Key)(fs: P1)(xs: P2)
        (implicit h1: HListerAux[P1, L1], h2: HListerAux[P2, L2], e: CoNatTRelAux[L1, Field, L2, Id]): Operation[L2] =
      set(k, h1(fs), h2(xs))

    def update[P1 <: Product, P2 <: Product, L1 <: HList, L2 <: HList, F](k: Key)(fs: P1)                                             
      (implicit
        h1: HListerAux[P1, L1],
        e: CoNatTRelAux[L1, Field, L2, Id],
        t: TuplerAux[L2, P2],
        h2: HListerAux[P2, L2],
        unhl : FnUnHListerAux[L2 => P2, F],
        hl : FnHListerAux[F, L2 => P2]) =
      new Update[L1, L2, P2, F](k, h1(fs))                                                                                            
                                                                                                                                                         
    class Update[L1 <: HList, L2 <: HList, P2 <: Product, F](k: Key, fs: L1)
      (implicit
        hl : FnHListerAux[F, L2 => P2], 
        h2: HListerAux[P2, L2],
        e : CoNatTRelAux[L1, Field, L2, Id],
        t: TuplerAux[L2, P2]) {
      def to(ts: F) = {
        val hts = (l2 : L2) => (h2(hl(ts).apply(l2)))
        update(k, fs)(hts)(e)
      }
    } 

    /*
    def update[P1 <: Product, P2 <: Product, L1 <: HList, L2 <: HList](k: Key)(fs: P1)
        (implicit h1: HListerAux[P1, L1], e: CoNatTRelAux[L1, Field, L2, Id], h2: HListerAux[P2, L2]) = {
      new Update[L1, L2, P2](k, h1(fs))
    }

    class Update[L1 <: HList, L2 <: HList, P2 <: Product](k: Key, fs: L1) {
      def to(ts: P2 => P2)(implicit h: HListerAux[P2, L2]) = update(k, fs)(ts.hlisted)
    }
    */
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
