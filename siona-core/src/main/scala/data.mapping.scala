//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data
package mapping

import siona.core.entity._

import repository._
import query._
import model._

trait Input[E <: Entity[_, _]] extends model.Input {
  def key: E#Key
}
trait Output[E <: Entity[_, _]] extends model.Output

trait Mapped[E <: Entity[_,_], T] extends model.Field[T] {
  val lens: scalaz.Lens[E, T]
  object Lens {
    def apply(a: E => T, b: E => T => E) = scalaz.Lens.lensG[E, T](a, b)
  }
}

trait Mapper[E <: Entity[_, _]] extends model.Model {
  override type In = Input[E]
  override type Out = Output[E]

  type Mapped[T] = mapping.Mapped[E, T]

  protected[this] def key[K, T](implicit in: In): Entity[K, T]#Key = in.key.asInstanceOf[Entity[K, T]#Key]

  protected def in(implicit in: In): E
  protected def out: List[Mapped[_]] // FIXME ... with HList? + Add key automatically

  def read(i: In) = in(i)
  def write(o: Out, e: E) = { // FIXME RETURN IO
    //out.foreach(f => f.apply(f.lens.get(e), o)) // FIX WITH HLIST
    e
  }

  implicit def fieldDefault2apply[T](f: model.Default[T])(implicit in: In) = f(in)
  implicit def fieldOptional2apply[T](f: model.Optional[T])(implicit in: In) = f(in)
}

trait Mapping[M <: Mapper[E], E <: Entity[K, T], K, T] extends Repository[M] {
  override type Key = E#Key

  implicit def e2key(e: E): Key = e.key

  implicit def e2ops(e: E) = EntityOps(e)
  
  case class EntityOps(e: E) {
    def save() = entity.set(e, e)
    def update[T](m: Mapper[E]#Mapped[T])(f: T => T) = entity.update(e, m)(f)
  }

  object entity {
    def get(k: Key) = Mapping.this.get(k, i => model.read(i))
    def set(k: Key, e: E) = Mapping.this.set(k, o => model.write(o, e))
    def update(k: Key, f: E => E) = Mapping.this.get(k, i => model.read(i)).flatMap(e => Mapping.this.set(k, o => model.write(o, e)))
    def update[T](e: E, m: Mapper[E]#Mapped[T])(f: T => T) = None
    def update[T0, T1](e: E)(ms: (Mapper[E]#Mapped[T0], Mapper[E]#Mapped[T1]))(ts: (T0, T1) => (T0, T1)) = None
    def query[List[E]](xs: Predicate[_, Indexed]) = None
  }
}

case class Entities[M <: Mapper[E], E <: Entity[K, T], K, T](override val model: M)
    extends Repository[M] with Mapping[M, E, K, T] {
  val key = new Field[K] {
    val name = "key"
    override type V = T
    def apply(implicit in: siona.data.model.Input): V = throw new Error("Key field cannot be read.")
  }
}
