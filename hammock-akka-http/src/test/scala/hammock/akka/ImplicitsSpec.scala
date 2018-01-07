package hammock
package akka

import _root_.akka.http.scaladsl.marshalling.Marshalling.Opaque
import _root_.akka.http.scaladsl.model.MessageEntity
import _root_.akka.http.scaladsl.model.HttpEntity
import _root_.akka.actor.ActorSystem
import _root_.akka.stream.ActorMaterializer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import org.scalatest._
import cats.implicits._

class ImplicitsSpec extends WordSpec with Implicits with Matchers {

  implicit val system = ActorSystem("test")
  implicit val mat    = ActorMaterializer()

  implicit val stringEncoder: Encoder[String] = new Encoder[String] {
    def encode(str: String): Entity =
      Entity.StringEntity(str)
  }

  implicit val byteArrayEncoder: Encoder[Array[Byte]] = new Encoder[Array[Byte]] {
    def encode(arr: Array[Byte]): Entity =
      Entity.ByteArrayEntity(arr)
  }

  implicit val stringDecoder: Decoder[String] = new Decoder[String] {
    def decode(a: Entity) = a match {
      case Entity.StringEntity(body, _) => body.asRight
      case _                            => CodecException.withMessage("boo").asLeft
    }

  }

  implicit val byteArrayDecoder: Decoder[Array[Byte]] = new Decoder[Array[Byte]] {
    def decode(a: Entity) = a match {
      case Entity.ByteArrayEntity(bytes, _) => bytes.asRight
      case _                                => CodecException.withMessage("boo").asLeft
    }
  }

  "Encoder[A] -> ToEntitymarshaller[A]" should {

    "work with Encoder[String]" in {

      Await
        .result(encoderToEntityMarshaller[String].apply("test"), Duration.Inf)
        .map(_.asInstanceOf[Opaque[MessageEntity]].marshal()) foreach { x =>
        x shouldEqual HttpEntity("test")
      }

    }

    "work with Encoder[Array[Byte]]" in {
      val arr = Array[Byte](1, 2, 3, 4)

      Await
        .result(encoderToEntityMarshaller[Array[Byte]].apply(arr), Duration.Inf)
        .map(_.asInstanceOf[Opaque[MessageEntity]].marshal()) foreach { x =>
        x shouldEqual HttpEntity(arr)
      }

    }

  }

  "Decoder[A] -> Fromentityunmarshaller[A]" should {

    "work with Decoder[String]" in {
      Await
        .result(stringDecoderFromEntityUnmarshaller.apply(HttpEntity("test")), Duration.Inf) shouldEqual "test"
    }

    "work with Decoder[Array[Byte]]" in {
      val arr = Array[Byte](1, 2, 3, 4)
      Await
        .result(byteArrayDecoderFromEntityUnmarshaller.apply(HttpEntity(arr)), Duration.Inf) shouldEqual arr
    }

  }

}
