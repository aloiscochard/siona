//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import shapeless._

import io._

package object record {
  /*case*/ abstract class Record[L <: HList, T](fields: L) extends io.Readable[T] with io.Writable[T] {
    def read(input: io.Input): T
    def write(out: io.Output, e: T) = e
  }
}
