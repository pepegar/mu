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

package freestyle.rpc.idlgen.avro

import freestyle.rpc.idlgen._
import freestyle.rpc.protocol._
import freestyle.rpc.internal.util._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import skeuomorph.freestyle.{SerializationType => ST, Protocol => FreesProtocol, _}
import skeuomorph.avro._
import qq.droste._

object AvroIdlGenerator extends AvroIdlGenerator {
  val serializationType: SerializationType = Avro
}

object AvroWithSchemaIdlGenerator extends AvroIdlGenerator {
  val serializationType: SerializationType = AvroWithSchema
}

trait AvroIdlGenerator extends IdlGenerator {

  val idlType: String       = avro.IdlType
  val outputSubdir: String  = "avro"
  val fileExtension: String = AvprExtension

  def generateFrom[T](rpc: FreesProtocol[T])(implicit T: Basis[FreesF, T]): List[String] = {
    val messages = rpc.declarations
    if (messages.nonEmpty)
      Protocol.toJson(Protocol.fromFreesFProtocol(rpc)).spaces2.split("\n").toList
    else Nil
  }
}
