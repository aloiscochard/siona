//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import siona.core.uuid._
import siona.core.entity._

package object io {
  trait Writable[T] extends Marshalling[T] {
    def write(out: Output, v: T): T
  }

  trait Readable[T] extends Unmarshalling[T] {
    def read(input: Input): T
  }

  trait Input {
    def read[T : Serializable](n: String): Option[T]
  }

  trait Output {
    def write[T : Serializable](n: String, x: T): T
  }

  trait Marshalling[T] { self: Writable[T] =>
    def >>[F <: Format](x: T)(implicit serializer: Serializer[F]) = marshall(x)
    def marshall[F <: Format](x: T)(implicit serializer: Serializer[F]) = serializer.marshall(self, x)
  }

  trait Unmarshalling[T] { self: Readable[T] =>
    def <<[F <: Format](implicit serializer: Serializer[F]) = unmarshall
    def unmarshall[F <: Format](implicit serializer: Serializer[F]) = serializer.unmarshall(self)
  }

  // TODO [aloiscochard] Replace Array[Byte] with ByteString
  case class Serializable[T](toBytes: T => Array[Byte], fromBytes: Array[Byte] => T)

  // TODO [aloiscochard] Check what happen when calling wrap(x).* with array of smaller size than needed,
  // maybe use as*Buffer for security
  object Serializable {
    import java.nio.ByteBuffer._

    implicit val boolean: Serializable[Boolean] = 
      Serializable(x => Array((if (x) 1 else 0): Byte), x => (x.size == 1 && x(0) == 1))

    implicit val char: Serializable[Char] =
      Serializable(x => allocate(2).putChar(x).array(), x => wrap(x).getChar)

    implicit val double: Serializable[Double] =
      Serializable(x => allocate(8).putDouble(x).array(), x => wrap(x).getDouble)

    implicit val float: Serializable[Float] =
      Serializable(x => allocate(4).putFloat(x).array(), x => wrap(x).getFloat)

    implicit val integer: Serializable[Int] =
      Serializable(x => allocate(4).putInt(x).array(), x => wrap(x).getInt)

    implicit val long: Serializable[Long] =
      Serializable(x => allocate(8).putLong(x).array(), x => wrap(x).getLong)

    implicit val short: Serializable[Short] =
      Serializable(x => allocate(2).putShort(x).array(), x => wrap(x).getShort)

    implicit val string: Serializable[String] =
      Serializable(_.getBytes, new String(_: Array[Byte]))

    implicit val uuid: Serializable[UUID] = 
      Serializable(_.toString.getBytes, (x: Array[Byte]) => java.util.UUID.fromString(new String(x)))
  }

  trait Serializer[F <: Format] {
    def marshall[T](s: Writable[T], x: T): Marshalled[T]
    def unmarshall[T](r: Readable[T]): Unmarshalled[T]

    case class Marshalled[T](bytes: Stream[Byte]) {
      def toBytes: Stream[Byte] = bytes
    }

    case class Unmarshalled[T](f: Stream[Byte] => T) {
      def fromBytes(bytes: Stream[Byte]): T = f(bytes)
    }
  }

  trait Format
  case class JSON() extends Format
  case class Protobuf() extends Format
}
package io {
  package object jackson {
    import java.io.ByteArrayOutputStream
    import com.fasterxml.jackson.core.JsonFactory
    import com.fasterxml.jackson.core.JsonGenerator
    import com.fasterxml.jackson.core.JsonEncoding

    implicit object JacksonSerializer extends Serializer[JSON] {
      def marshall[T](s: Writable[T], x: T): Marshalled[T] = { // Return IO?
        val os = new ByteArrayOutputStream()// TODO Size, calculated?
        val jg = new JsonFactory().createJsonGenerator(os, JsonEncoding.UTF8) // TODO Encoding;
        jg.writeStartObject
        s.write(Generator(jg), x)
        jg.writeEndObject
        jg.close
        os.close
        Marshalled[T](os.toByteArray.toStream)
      }

      def unmarshall[T](r: Readable[T]): Unmarshalled[T] = Unmarshalled(null)
    }

    case class Generator(jg: JsonGenerator) extends Output {
      def write[T](name: String, value: T)(implicit ser: Serializable[T]): T = {
        value match {
          case x: Boolean => jg.writeBooleanField(name, x)
          case x: String => jg.writeStringField(name, x)
          case x => jg.writeBinaryField(name, ser.toBytes(x))
        }
        value
      }
    }
  }
}
