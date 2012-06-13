//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data
package io

import siona.core.uuid._

import org.specs2.mutable._

class DataIOSpec extends Specification {
  "siona-data-io" should {
    "serialize standard types" in {
      def check[T](x: T)(implicit s: Serializable[T]) = s.get(s.put(x))

      val boolean: Boolean = true
      val character: Char = 'z'
      val double: Double = 7.9
      val float: Float = 9893.823f
      val integer: Int = 65535
      val long: Long = 8798783
      val short: Short = 16
      val string: String = "hello siona"
      val uuid: UUID = UUID.random

      check(boolean) must beEqualTo(boolean)
      check(character) must beEqualTo(character)
      check(double) must beEqualTo(double)
      check(float) must beEqualTo(float)
      check(integer) must beEqualTo(integer)
      check(long) must beEqualTo(long)
      check(short) must beEqualTo(short)
      check(string) must beEqualTo(string)
      check(uuid) must beEqualTo(uuid)
    }
  }
}
