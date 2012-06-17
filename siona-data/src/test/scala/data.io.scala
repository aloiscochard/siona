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
      val uuid: UUID = UUID.generate

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

    "support native serialization" in {
      import io.native._

      object Data extends Writable[Data] with Readable[Data] {
        def write(out: Output, data: Data): Data = {
          out.write("string", data.string)
          data
        }

        def read(input: Input): Data = {
          new Data(input.read[String]("string").getOrElse(""))
        }
      }

      case class Data(val string: String)

      val i = new Data("hello siona! éàôäö")
      val o = Data.unmarshall[Native].fromBytes(Data.marshall[Native](i).toBytes)

      i must beEqualTo(o)
    }
  }
}
