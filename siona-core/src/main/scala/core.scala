//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.core

import scalaz._
import Scalaz._

package object uuid {
  // TODO [aloiscochard] Replace UUID with correct implementation
  type UUID = com.eaio.uuid.UUID

  object UUID {
    def generate = new UUID
    def fromString(s: String) = new UUID(s)
  }

  implicit val uuidEqual = new Equal[UUID] {
    def equal(id1: UUID, id2: UUID): Boolean = id1.compareTo(id2) == 0
  }

  implicit def uuidOps(uuid: UUID) = new UUIDOps(uuid)

  class UUIDOps(uuid: UUID) {

    // TODO [aloiscochard] Review this by a math nerd
    def shard(size: Int = 128): Int = {
      def f2(size: Int) = {
        def f(x: Int, y: Int): Int = (x % (y * 3)) / 3
        val hash = math.abs(uuid.hashCode)
        (1 to size).map(f(hash, _)).sum % size
      }
      f2(size * 8) % size
    }
  }
}

package object entity {
  object Key {
    def apply[K, T](k: K): Entity[K, T]#Key = k.asInstanceOf[Entity[K, T]#Key]
  }

  object Entity {
    implicit def equal[K, T](implicit eqK: Equal[K]): Equal[Entity[K, T]] = new Equal[Entity[K, T]] {
      def equal(e1: Entity[K, T], e2: Entity[K, T]): Boolean = e1.id === e2.id
    }
  }

  trait Entity[K, T] {
    type Key = K @@ T
    val key: Key
    def id: K = key.asInstanceOf[K]
  }

  implicit def entityOps[K, T](e: Entity[K, T]) = EntityOps(e) 
  
  case class EntityOps[K, T](val entity: Entity[K,T]) {
    def ===(other: Entity[K, T])(implicit e: Equal[Entity[K, T]]) = e.equal(entity, other)
  }

}

