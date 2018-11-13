/*
 * Copyright 2017-2018 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package mu.rpc
package protocol

import cats.Applicative
import cats.syntax.applicative._
import org.scalatest._
import mu.rpc.common._
import mu.rpc.internal.encoders.avro.bigDecimalTagged._
import mu.rpc.internal.encoders.avro.bigDecimalTagged.marshallers._
import mu.rpc.internal.encoders.pbd.bigDecimal._
import mu.rpc.testing.servers.withServerChannel
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import shapeless.{tag, Nat}
import shapeless.tag.@@

import scala.math.BigDecimal.RoundingMode

class RPCBigDecimalTests extends RpcBaseTestSuite with BeforeAndAfterAll with Checkers {

  private[this] def bigDecimalGen(scale: Int): Gen[BigDecimal] =
    Arbitrary
      .arbitrary[BigDecimal]
      .map(_.setScale(scale, RoundingMode.HALF_DOWN))

  implicit val requestPSArb: Arbitrary[RPCService.RequestPS] = Arbitrary {
    for {
      bd1   <- bigDecimalGen(2).map(tag[(Nat._8, Nat._2)][BigDecimal])
      bd2   <- bigDecimalGen(2).map(tag[((Nat._1, Nat._2), Nat._2)][BigDecimal])
      bd3   <- bigDecimalGen(12).map(tag[((Nat._2, Nat._0), (Nat._1, Nat._2))][BigDecimal])
      label <- Gen.alphaStr
    } yield RPCService.RequestPS(bd1, bd2, bd3, label)
  }

  object RPCService {

    case class Request(bigDecimal: BigDecimal, label: String)

    case class Response(bigDecimal: BigDecimal, result: String, check: Boolean)

    case class RequestPS(
        bd1: BigDecimal @@ (Nat._8, Nat._2),
        bd2: BigDecimal @@ ((Nat._1, Nat._2), Nat._2),
        bd3: BigDecimal @@ ((Nat._2, Nat._0), (Nat._1, Nat._2)),
        label: String)

    case class ResponsePS(
        bd1: BigDecimal @@ (Nat._8, Nat._2),
        bd2: BigDecimal @@ ((Nat._1, Nat._2), Nat._2),
        bd3: BigDecimal @@ ((Nat._2, Nat._0), (Nat._1, Nat._2)),
        result: String,
        check: Boolean)

    @service(Protobuf) trait ProtoRPCServiceDef[F[_]] {
      def bigDecimalProto(bd: BigDecimal): F[BigDecimal]
      def bigDecimalProtoWrapper(req: Request): F[Response]
    }
    @service(Avro) trait AvroRPCServiceDef[F[_]] {
      def bigDecimalAvro(bd: BigDecimal @@ (Nat._8, Nat._2)): F[BigDecimal @@ (Nat._8, Nat._2)]
      def bigDecimalAvroWrapper(req: RequestPS): F[ResponsePS]
    }
    @service(AvroWithSchema) trait AvroWithSchemaRPCServiceDef[F[_]] {
      def bigDecimalAvroWithSchema(
          bd: BigDecimal @@ (Nat._8, Nat._2)): F[BigDecimal @@ (Nat._8, Nat._2)]
      def bigDecimalAvroWithSchemaWrapper(req: RequestPS): F[ResponsePS]
    }

    class RPCServiceDefImpl[F[_]: Applicative]
        extends ProtoRPCServiceDef[F]
        with AvroRPCServiceDef[F]
        with AvroWithSchemaRPCServiceDef[F] {

      def bigDecimalProto(bd: BigDecimal): F[BigDecimal] = bd.pure

      def bigDecimalProtoWrapper(req: Request): F[Response] =
        Response(req.bigDecimal, req.label, check = true).pure

      def bigDecimalAvro(bd: BigDecimal @@ (Nat._8, Nat._2)): F[BigDecimal @@ (Nat._8, Nat._2)] =
        bd.pure

      def bigDecimalAvroWrapper(req: RequestPS): F[ResponsePS] =
        ResponsePS(req.bd1, req.bd2, req.bd3, req.label, check = true).pure

      def bigDecimalAvroWithSchema(
          bd: BigDecimal @@ (Nat._8, Nat._2)): F[BigDecimal @@ (Nat._8, Nat._2)] = bd.pure

      def bigDecimalAvroWithSchemaWrapper(req: RequestPS): F[ResponsePS] =
        ResponsePS(req.bd1, req.bd2, req.bd3, req.label, check = true).pure
    }

  }

  object RPCServiceWithImplicitRM {

    implicit val RM: RoundingMode.RoundingMode = RoundingMode.HALF_DOWN

    case class Request(bigDecimal: BigDecimal @@ (Nat._8, Nat._2), label: String)

    case class Response(bigDecimal: BigDecimal @@ (Nat._8, Nat._2), result: String, check: Boolean)

    @service(Avro) trait AvroRPCServiceDef[F[_]] {
      def bigDecimalAvro(bd: BigDecimal @@ (Nat._8, Nat._2)): F[BigDecimal @@ (Nat._8, Nat._2)]
      def bigDecimalAvroWrapper(req: Request): F[Response]
    }

    class RPCServiceDefImpl[F[_]: Applicative] extends AvroRPCServiceDef[F] {

      def bigDecimalAvro(bd: BigDecimal @@ (Nat._8, Nat._2)): F[BigDecimal @@ (Nat._8, Nat._2)] =
        bd.pure

      def bigDecimalAvroWrapper(req: Request): F[Response] =
        Response(req.bigDecimal, req.label, check = true).pure
    }

  }

  "A RPC server" should {

    import TestsImplicits._
    import RPCService._

    implicit val H: RPCServiceDefImpl[ConcurrentMonad] = new RPCServiceDefImpl[ConcurrentMonad]

    "be able to serialize and deserialize BigDecimal using proto format" in {

      withServerChannel(ProtoRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: ProtoRPCServiceDef.Client[ConcurrentMonad] =
          ProtoRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll { bd: BigDecimal =>
            client.bigDecimalProto(bd).unsafeRunSync() == bd
          }
        }

      }

    }

    "be able to serialize and deserialize BigDecimal in a Request using proto format" in {

      withServerChannel(ProtoRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: ProtoRPCServiceDef.Client[ConcurrentMonad] =
          ProtoRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll { (bd: BigDecimal, s: String) =>
            client.bigDecimalProtoWrapper(Request(bd, s)).unsafeRunSync() == Response(
              bd,
              s,
              check = true)
          }
        }

      }

    }

    "be able to serialize and deserialize BigDecimal using avro format" in {

      withServerChannel(AvroRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: AvroRPCServiceDef.Client[ConcurrentMonad] =
          AvroRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll(bigDecimalGen(2)) { bd =>
            client.bigDecimalAvro(tag[(Nat._8, Nat._2)][BigDecimal](bd)).unsafeRunSync() == bd
          }
        }

      }

    }

    "be able to serialize and deserialize BigDecimal in a Request using avro format" in {

      withServerChannel(AvroRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: AvroRPCServiceDef.Client[ConcurrentMonad] =
          AvroRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll { request: RequestPS =>
            client.bigDecimalAvroWrapper(request).unsafeRunSync() == ResponsePS(
              request.bd1,
              request.bd2,
              request.bd3,
              request.label,
              check = true)
          }
        }

      }

    }

    "be able to serialize and deserialize BigDecimal using avro with schema format" in {

      withServerChannel(AvroWithSchemaRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: AvroWithSchemaRPCServiceDef.Client[ConcurrentMonad] =
          AvroWithSchemaRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll(bigDecimalGen(2)) { bd =>
            client
              .bigDecimalAvroWithSchema(tag[(Nat._8, Nat._2)][BigDecimal](bd))
              .unsafeRunSync() == bd
          }
        }

      }

    }

    "be able to serialize and deserialize BigDecimal in a Request using avro with schema format" in {

      withServerChannel(AvroWithSchemaRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: AvroWithSchemaRPCServiceDef.Client[ConcurrentMonad] =
          AvroWithSchemaRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll { request: RequestPS =>
            client.bigDecimalAvroWithSchemaWrapper(request).unsafeRunSync() == ResponsePS(
              request.bd1,
              request.bd2,
              request.bd3,
              request.label,
              check = true)
          }
        }
      }
    }
  }

  "A RPC server with an implicit rounding mode" should {

    import TestsImplicits._
    import RPCServiceWithImplicitRM._

    implicit val H: RPCServiceDefImpl[ConcurrentMonad] = new RPCServiceDefImpl[ConcurrentMonad]

    "be able to serialize and deserialize BigDecimal using avro format" in {

      withServerChannel(AvroRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: AvroRPCServiceDef.Client[ConcurrentMonad] =
          AvroRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll(bigDecimalGen(12)) { bd =>
            client.bigDecimalAvro(tag[(Nat._8, Nat._2)][BigDecimal](bd)).unsafeRunSync() == bd
              .setScale(2, RM)
          }
        }

      }

    }

    "be able to serialize and deserialize BigDecimal in a Request using avro" in {

      withServerChannel(AvroRPCServiceDef.bindService[ConcurrentMonad]) { sc =>
        val client: AvroRPCServiceDef.Client[ConcurrentMonad] =
          AvroRPCServiceDef.clientFromChannel[ConcurrentMonad](sc.channel)

        check {
          forAll(bigDecimalGen(12)) { bd =>
            val bdTagged = tag[(Nat._8, Nat._2)][BigDecimal](bd)
            client
              .bigDecimalAvroWrapper(Request(tag[(Nat._8, Nat._2)][BigDecimal](bd), "label"))
              .unsafeRunSync() == Response(
              tag[(Nat._8, Nat._2)][BigDecimal](bd.setScale(2, RM)),
              "label",
              check = true)
          }
        }
      }
    }
  }
}
