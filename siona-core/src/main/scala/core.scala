//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.core

import scalaz._
import Scalaz._

package object uuid {
  // TODO [aloiscochard] Replace UUID with correct implementation
  type UUID = java.util.UUID

  object UUID {
    def random = java.util.UUID.randomUUID
  }

  implicit val uuidEqual = new Equal[UUID] {
    def equal(id1: UUID, id2: UUID): Boolean = id1 == id2
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

