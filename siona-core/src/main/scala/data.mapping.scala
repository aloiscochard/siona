//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data
package mapping

import scalaz._
import scalaz.effect._

import siona.core.entity._

import io._
import repository._
import query._
import model._

trait Mapped[E <: Entity[_,_], T] { self: model.Field[T] =>
  def read(x: T)(implicit out: Output, s: Serializable[T]): IO[T] = self.apply(x)
  val lens: LensT[Id, E, T]
}


trait Mapper[E <: Entity[_, _]] extends model.Model with io.Readable[E] with io.Writable[E] {

  type Lens[T] = LensT[Id, E, T]

  object Lens {
    def apply[T](set: E => T => E, get: E => T) = scalaz.Lens.lensg[E, T](set, get)
  }

  type Mapped[T] = mapping.Mapped[E, T]

  implicit def e2w(entity: E) = Marshable(entity)

  case class Marshable(entity: E) {
    def marshall[F <: Format](implicit serializer: Serializer[F]) = serializer.marshall(Mapper.this, entity)
  }

  case class KeyField[K](override val lens: LensT[Id, E, K]) extends MapperKeyField[K, E]

  trait MapperKeyField[K, T] extends Field[K] with Mapped[K] {
    override val name: String = "id"
    override type V = Entity[K, T]#Key
    def apply(implicit in: siona.data.io.Input, s: Serializable[K]): V = // Add monoid for default/ or a id gen typeclass
      in.read[K](name).get.asInstanceOf[Entity[K, T]#Key] // evil
  }

  protected def in(implicit in: io.Input): E
  protected def out(implicit out: io.Output): List[(Mapped[_], _ => IO[_])]

  // def schema

  def read(i: io.Input) = in(i)
  def write(o: io.Output, e: E) = { // FIXME RETURN IO
    out(o).foreach { x => 
      val (field, f) = x
      (f.asInstanceOf[Any => IO[Any]])(field.lens.get(e)).unsafePerformIO
    }
    e
  }

  implicit def mapped2io[T](f: Mapped[T])(implicit out: io.Output, s: Serializable[T]): (Mapped[T], T => IO[T]) =
    f -> (f.read(_: T)(out, s))

  //implicit def fieldKey2apply[K, T](f: KeyField[K, T])(implicit in: io.Input, s: Serializable[K]): KeyField[K, T]#V = f(in, s)
  implicit def fieldKey2apply[K](f: KeyField[K])(implicit in: io.Input, s: Serializable[K]): KeyField[K]#V = f(in, s)
  implicit def fieldDefault2apply[T](f: model.Default[T])(implicit in: io.Input, s: Serializable[T]) = f(in, s)
  implicit def fieldOptional2apply[T](f: model.Optional[T])(implicit in: io.Input, s: Serializable[T]) = f(in, s)
}

trait MappedDocument[E <: Entity[_, _]] extends Document with Mapper[E] {

  case class MappedField[T](
    override val name: String,
    override val validation: Validator[T],
    override val lens: LensT[Id, E, T]
  )(implicit val m: Monoid[T]) extends DocumentField[T] with Mapped[T]
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
}
