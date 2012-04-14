//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package object siona {
  import siona.core._
  // core.uuid
  type UUID = uuid.UUID
  val UUID = uuid.UUID
  implicit val uuidEqual = uuid.uuidEqual
  // core.entity
  val Key = entity.Key
  type Entity[K, T] = entity.Entity[K, T]
  implicit def entityOps[K, T](e: entity.Entity[K, T]) = entity.entityOps(e) 
}
