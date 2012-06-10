//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import siona.core.uuid._
import siona.core.entity._

import model._

package object io {
  trait Writable[T] extends Marshalling[T] {
    def write(out: Output, v: T): T
  }

  trait Readable[T] extends Unmarshalling[T] {
    def read(input: Input): T
  }

  trait Input {
    def apply[T : Serializable](f: Field[T]) = f.apply(this, implicitly[Serializable[T]])
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

  case class Serializable[T](toBytes: T => Array[Byte], fromBytes: Array[Byte] => T)

  object Serializable {
    implicit val uuid: Serializable[UUID] = 
      Serializable(_.toString.getBytes, (x: Array[Byte]) => java.util.UUID.fromString(new String(x)))
    implicit val string: Serializable[String] =
      Serializable(_.getBytes, new String(_: Array[Byte]))
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
