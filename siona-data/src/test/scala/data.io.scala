//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data
package io

import siona.core.uuid._

import org.specs2.mutable._

class DataIOSpec extends Specification {

  val boolean: Boolean = true
  val byte: Byte = 2
  val bytes: Array[Byte] = Array(2)
  val character: Char = 'z'
  val double: Double = 7.9
  val float: Float = 9893.823f
  val integer: Int = 65535
  val long: Long = 8798783
  val short: Short = 16
  val string: String = "hello siona"
  val uuid: UUID = UUID.generate

  "siona-data-io" should {
    "serialize standard types" in {
      def check[T](x: T)(implicit s: Serializable[T]) = s.get(s.put(x))

      check(boolean) must beEqualTo(boolean)
      check(byte) must beEqualTo(byte)
      check(bytes) must beEqualTo(bytes)
      check(character) must beEqualTo(character)
      check(double) must beEqualTo(double)
      check(float) must beEqualTo(float)
      check(integer) must beEqualTo(integer)
      check(long) must beEqualTo(long)
      check(short) must beEqualTo(short)
      check(string) must beEqualTo(string)
      check(uuid) must beEqualTo(uuid)
    }

    "support native strict serialization" in {
      import io.native._

      serializerStrictSpec(serializer[Native])
    }
  }

  def serializerSpec[F <: Format](s: Serializer[F]) = {
    serializerStrictSpec(s)
    s.read[Array[Byte]]("b").fromBytes(s.write("b", bytes).toBytes) must beEqualTo(Some(bytes))
  }

  def serializerStrictSpec[F <: Format](s: Serializer[F]) = {
    s.read[Boolean]("b").fromBytes(s.write("b", boolean).toBytes) must beEqualTo(Some(boolean))
    s.read[Byte]("b").fromBytes(s.write("b", byte).toBytes) must beEqualTo(Some(byte))
    s.read[Char]("c").fromBytes(s.write("c", character).toBytes) must beEqualTo(Some(character))
    s.read[Double]("d").fromBytes(s.write("d", double).toBytes) must beEqualTo(Some(double))
    s.read[Float]("f").fromBytes(s.write("f", float).toBytes) must beEqualTo(Some(float))
    s.read[Int]("i").fromBytes(s.write("i", integer).toBytes) must beEqualTo(Some(integer))
    s.read[Long]("l").fromBytes(s.write("l", long).toBytes) must beEqualTo(Some(long))
    s.read[Short]("s").fromBytes(s.write("s", short).toBytes) must beEqualTo(Some(short))
    s.read[String]("s").fromBytes(s.write("s", string).toBytes) must beEqualTo(Some(string))
    s.read[UUID]("u").fromBytes(s.write("u", uuid).toBytes) must beEqualTo(Some(uuid))
  }
}
