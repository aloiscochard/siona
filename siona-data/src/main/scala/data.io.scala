//  ____,__, ____, _,  _, ____,
// (-(__(-| (-/  \(-|\ | (-/_| 
//  ____)_|_,_\__/,_| \|,_/  |,
//
package siona.data

import siona.core.uuid._
import siona.core.entity._

package object io {
  import bytestring._
  
  implicit def marshallingOps[T](x: T)(implicit m: Marshalling[T]) = new MarshallingOps(x, m)

  class MarshallingOps[T](x: T, m: Marshalling[T]) {
    def marshall[F <: Format]()(implicit s: Serializer[F]) = m.marshall(x)
  }

  def unmarshall[T, F <: Format](implicit u: Unmarshalling[T], s: Serializer[F]) = u.unmarshall

  def serializer[F <: Format](implicit s: Serializer[F]) = s


  // TODO Move to a specific package and add support for Map
  implicit def tupleWriter[T](implicit s: Serializable[T]) = new TupleWriter[T]
  class TupleWriter[T](implicit s: Serializable[T]) extends Writable[(String, T)] {

    def write(out: Output, tuple: (String, T)): (String, T) = {
      out.write(tuple._1, tuple._2)
      tuple
    }
  }

  trait Persistable[T] extends Writable[T] with Readable[T]

  trait Writable[T] extends Marshalling[T] {
    def write(out: Output, v: T): T
  }

  trait Readable[T] extends Unmarshalling[T] {
    def read(input: Input): T // TODO Option or Validation?
  }

  // TODO Validation + IO
  trait Input {
    def read[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Option[T] // TODO Use validation somewhere to catch errors
    def readList[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Seq[T]
    def readOption[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Option[T]
    def readObject[T](n: String)(implicit r: Readable[T]): Option[T]
    def readObjectList[T](n: String)(implicit r: Readable[T]): Seq[T]
    def readObjectOption[T](n: String)(implicit r: Readable[T]): Option[T]
  }

  trait Output {
    def write[T](n: String, x: T)(implicit s: Serializable[T]): T
    def writeOption[T](n: String, x: Option[T])(implicit s: Serializable[T]): Option[T]
    def writeList[T](n: String, xs: Seq[T])(implicit s: Serializable[T]): Seq[T]
    def writeObject[T](n: String, x: T)(implicit w: Writable[T]): T
    def writeObjectList[T](n: String, xs: Seq[T])(implicit w: Writable[T]): Seq[T]
    def writeObjectOption[T](n: String, x: Option[T])(implicit w: Writable[T]): Option[T]
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
      fromByteArray(x => Array((if (x) 1 else 0): Byte), x => x(0) == 1)

    implicit val byte: Serializable[Byte] = 
      fromByteArray(x => Array(x), x => x(0))

    implicit val bytes: Serializable[Array[Byte]] = 
      fromByteArray(identity, identity)

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
      fromByteArray(_.toString.getBytes, (x: Array[Byte]) => UUID.fromString(new String(x)))

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

    def read[T](name: String)(implicit s: Serializable[T], m: Manifest[T]) =
      new SimpleReader[T](name).unmarshall(this)

    // TODO WIP
    //def readList[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Seq[T] =
    //def readOption[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Option[T] =

    def write[T](name: String, x: T)(implicit s: Serializable[T], m: Manifest[T]) =
      new SimpleWriter[T](name).marshall(x)(this)


    class SimpleReader[T](name: String)(implicit s: Serializable[T], m: Manifest[T]) extends Readable[Option[T]] {
      def read(input: Input): Option[T] = input.read[T](name)
    }

    class SimpleWriter[T](name: String)(implicit s: Serializable[T], m: Manifest[T]) extends Writable[T] {
      def write(out: Output, v: T): T = out.write(name, v)
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

    // TODO [aloiscochard] Add native version that use field name and rename this one to "strict"

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
      
      private class NativeInput(ois: ObjectInputStream) extends Input {
        def read[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Option[T] = (
          if (m <:< manifest[Boolean]) Some(ois.readBoolean)
          else if (m <:< manifest[Char]) Some(ois.readChar)
          else if (m <:< manifest[Byte]) Some(ois.readByte)
          else if (m <:< manifest[Char]) Some(ois.readChar)
          else if (m <:< manifest[Double]) Some(ois.readDouble)
          else if (m <:< manifest[Float]) Some(ois.readFloat)
          else if (m <:< manifest[Int]) Some(ois.readInt)
          else if (m <:< manifest[Long]) Some(ois.readLong)
          else if (m <:< manifest[Short]) Some(ois.readShort)
          else if (m <:< manifest[String]) Some(ois.readUTF)
          else if (m <:< manifest[UUID]) Some(UUID.fromString(ois.readUTF))
          else None
        ).asInstanceOf[Option[T]]

        def readList[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Seq[T] =
          read[Int]("").map(size => (1 to size).flatMap(_ => read[T](""))).getOrElse(Nil)

        def readOption[T](n: String)(implicit s: Serializable[T], m: Manifest[T]): Option[T] =
          read[Boolean]("").flatMap(defined => if (defined) read[T]("") else None)

        def readObject[T](n: String)(implicit r: Readable[T]): Option[T] = Some(r.read(this))

        def readObjectList[T](n: String)(implicit r: Readable[T]): Seq[T] =
          read[Int]("").map(size => (1 to size).flatMap(_ => readObject[T](""))).getOrElse(Nil)

        def readObjectOption[T](n: String)(implicit r: Readable[T]): Option[T] =
          read[Boolean]("").flatMap(defined => if (defined) readObject[T]("") else None)
      }

      private class NativeOutput(oos: ObjectOutputStream) extends Output {
        def write[T](name: String, value: T)(implicit s: Serializable[T]): T = {
          value match {
            case x: Boolean => oos.writeBoolean(x)
            case x: Char => oos.writeChar(x)
            case x: Double => oos.writeDouble(x)
            case x: Float => oos.writeFloat(x)
            case x: Int => oos.writeInt(x)
            case x: Long => oos.writeLong(x)
            case x: Short => oos.writeShort(x)
            case x: String => oos.writeUTF(x)
            case x: UUID => oos.writeUTF(x.toString)
            case x => oos.write(s.put(x).toArray)
          }
          value
        }

        def writeList[T](n: String, xs: Seq[T])(implicit w: Serializable[T]): Seq[T] = {
          write[Int]("", xs.size)
          xs.map(write("", _))
        }

        def writeOption[T](n: String, x: Option[T])(implicit s: Serializable[T]): Option[T] = {
          write[Boolean]("", x.isDefined)
          x.map(write("", _))
        }

        def writeObject[T](n: String, x: T)(implicit w: Writable[T]): T = w.write(this, x)

        def writeObjectList[T](n: String, xs: Seq[T])(implicit w: Writable[T]): Seq[T] = {
          write[Int]("", xs.size)
          xs.map(writeObject("", _))
        }

        def writeObjectOption[T](n: String, x: Option[T])(implicit w: Writable[T]): Option[T] = {
          write[Boolean]("", x.isDefined)
          x.map(w.write(this, _))
        }
      }
    }
  }

  /*
  package object jackson {
    import java.io.ByteArrayOutputStream
    import com.fasterxml.jackson.core.JsonFactory
    import com.fasterxml.jackson.core.JsonGenerator
    import com.fasterxml.jackson.core.JsonEncoding
    
    // TODO [aloiscochard] add List support
    // TODO [aloiscochard] add Option support

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
  */
}
