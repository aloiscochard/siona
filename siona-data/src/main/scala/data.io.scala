//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import siona.core.uuid._
import siona.core.entity._

package object io {
  import bytestring._
  
  trait Writable[T] extends Marshalling[T] {
    def write(out: Output, v: T): T
  }

  trait Readable[T] extends Unmarshalling[T] {
    def read(input: Input): T
  }

  trait Input {
    def read[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Option[T]
  }

  trait Output {
    def write[T](n: String, x: T)(implicit s: Serializable[T]): T
  }

  trait Marshalling[T] { self: Writable[T] =>
    def >>[F <: Format](x: T)(implicit serializer: Serializer[F]) = marshall(x)
    def marshall[F <: Format](x: T)(implicit serializer: Serializer[F]) = serializer.marshall(self, x)
  }

  trait Unmarshalling[T] { self: Readable[T] =>
    def <<[F <: Format](implicit serializer: Serializer[F]) = unmarshall
    def unmarshall[F <: Format](implicit serializer: Serializer[F]) = serializer.unmarshall(self)
  }

  case class Serializable[T](put: T => ByteString, get: ByteString => T)

  // TODO [aloiscochard] Check what happen when calling wrap(x).* with array of smaller size than needed,
  // maybe use as*Buffer for security + check where ByteString could be use directly instead of Array[Byte]
  // TODO [aloiscochard] Use put/get monads when available in Scalaz
  object Serializable {
    import java.nio.ByteBuffer._

    def fromByteArray[T](toBytes: T => Array[Byte], fromBytes: Array[Byte] => T): Serializable[T] =
      Serializable(x => new ByteString(toBytes(x)), x => fromBytes(x.toArray))

    implicit val boolean: Serializable[Boolean] = 
      fromByteArray(x => Array((if (x) 1 else 0): Byte), x => (x.size == 1 && x(0) == 1))

    // TODO [aloiscochard] Missing byte and bytes!

    implicit val char: Serializable[Char] =
      fromByteArray(x => allocate(2).putChar(x).array(), x => wrap(x).getChar)

    implicit val double: Serializable[Double] =
      fromByteArray(x => allocate(8).putDouble(x).array(), x => wrap(x).getDouble)

    implicit val float: Serializable[Float] =
      fromByteArray(x => allocate(4).putFloat(x).array(), x => wrap(x).getFloat)

    implicit val integer: Serializable[Int] =
      fromByteArray(x => allocate(4).putInt(x).array(), x => wrap(x).getInt)

    implicit val long: Serializable[Long] =
      fromByteArray(x => allocate(8).putLong(x).array(), x => wrap(x).getLong)

    implicit val short: Serializable[Short] =
      fromByteArray(x => allocate(2).putShort(x).array(), x => wrap(x).getShort)

    implicit val string: Serializable[String] =
      fromByteArray(_.getBytes, new String(_: Array[Byte]))

    implicit val uuid: Serializable[UUID] = 
      fromByteArray(_.toString.getBytes, (x: Array[Byte]) => new UUID(new String(x)))

  }

  // TODO [aloiscochard] Use ByteSring instead of Stream[Byte]? or maybe better data-structure
  // Take a look at https://github.com/arjanblokzijl/scala-conduits
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

  // TODO [aloiscochard] Add output type in format (Binary extends Format[ByteString], JSON -> String, Protobuf ext Binary)
  trait Format
  trait Binary extends Format
  trait JSON extends Format
  trait Protobuf extends Format

}
package io {
  package object native {
    import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
    import java.io.{ObjectOutputStream, ObjectInputStream}

    // TODO [aloiscochard] Add native version that use field name and rename this one

    class Native extends Binary

    implicit object NativeSerializer extends Serializer[Native] {
      def marshall[T](s: Writable[T], x: T): Marshalled[T] = { // Return IO?
        val bos = new ByteArrayOutputStream()// TODO Size, calculated?
        val oos = new ObjectOutputStream(bos)

        s.write(new NativeOutput(oos), x)

        // TODO usefull?
        oos.close
        bos.close

        Marshalled[T](bos.toByteArray.toStream)
      }

      def unmarshall[T](r: Readable[T]): Unmarshalled[T] = Unmarshalled(
        input => {
          val bis = new ByteArrayInputStream(input.toArray)
          val ois = new ObjectInputStream(bis)

          val x = r.read(new NativeInput(ois))

          // TODO usefull?
          bis.close
          ois.close

          x
        }
      )
      
      private class NativeOutput(oos: ObjectOutputStream) extends Output {
        def write[T](name: String, value: T)(implicit s: Serializable[T]): T = {
          value match {
            case x: Boolean => oos.writeBoolean(x)
            case x: String => oos.writeUTF(x)
            case x: Char => oos.writeChar(x)
            case x: Double => oos.writeDouble(x)
            case x: Float => oos.writeFloat(x)
            case x: Int => oos.writeInt(x)
            case x: Long => oos.writeLong(x)
            case x: Short => oos.writeShort(x)
            case x: UUID => oos.writeChars(x.toString)
            case x => oos.write(s.put(x).toArray)
          }
          value
        }
      }

      private class NativeInput(ois: ObjectInputStream) extends Input {
        def read[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Option[T] = {
          if (m <:< manifest[Boolean]) Some(ois.readBoolean).asInstanceOf[Option[T]]
          // TODO [aloiscochard] WIP
          else if (m <:< manifest[String]) Some(ois.readUTF).asInstanceOf[Option[T]]
          else None
        }
      }
    }
  }

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
          case x: UUID => jg.writeStringField(name, x.toString)
          case x => jg.writeBinaryField(name, ser.put(x).toArray)
        }
        value
      }
    }
  }
}
