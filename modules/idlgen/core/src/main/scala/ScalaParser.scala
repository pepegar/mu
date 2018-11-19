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

package freestyle.rpc.idlgen

import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

import freestyle.rpc.protocol.{Avro, AvroWithSchema, Protobuf, SerializationType}
import freestyle.rpc.internal.util.StringUtil._
import freestyle.rpc.internal.util._
import skeuomorph.freestyle.{SerializationType => ST, _}
import qq.droste._
import monocle.Iso

object ScalaParser {

  import Toolbox.u._
  import AstOptics._

  val greeterService =
    "/home/pepe/projects/frees-io/freestyle-rpc/modules/idlgen/core/src/test/resources/GreeterService.scala"

  val Tree: Tree =
    Toolbox.parse(Files.readAllLines(Paths.get(greeterService)).asScala.mkString("\n"))

  def parse[T](input: Tree, inputName: String)(implicit T: Basis[FreesF, T]): Protocol[T] = {
    val definitions = input.collect { case defs: ModuleDef => defs }
    def annotationValue(name: String): Option[String] =
      (for {
        defn       <- definitions
        annotation <- annotationsNamed(name).getAll(defn)
        firstArg   <- annotation.firstArg
      } yield firstArg).headOption.map(_.toString.unquoted)

    val outputName    = annotationValue("outputName").getOrElse(inputName)
    val outputPackage = annotationValue("outputPackage")
    val options: List[(String, String)] = for {
      defn             <- definitions
      option           <- annotationsNamed("option").getAll(defn)
      Seq(name, value) <- option.withArgsNamed("name", "value")
    } yield (name.toString.unquoted, value.toString) // keep value quoting as-is

    def declarations: List[T] =
      for {
        defn <- input.collect {
          case ast._CaseClassDef(tree) if hasAnnotation("message")(tree) => tree
        }
      } yield parseType.apply(defn)

    def getRequestsFromService(defn: Tree): List[Service.Operation[T]] =
      for {
        x           <- defn.collect({ case ast._DefDef(x) if x.rhs.isEmpty => x })
        name        <- List(x.name.toString)
        requestType <- firstParamType.getOption(x).toList
        responseType = returnType.get(x)
      } yield Service.Operation(name, parseType.apply(requestType), parseType.apply(responseType))

    def services: List[Service[T]] =
      input.collect {
        case ServiceClass(clazz, serializationType) =>
          Service(
            clazz.name.toString,
            serTypeIso.get(serializationType),
            getRequestsFromService(clazz))
      }

    Protocol(outputName, outputPackage, options, declarations, services)
  }

  val parseTypeAlgebra: Coalgebra[FreesF, Tree] = Coalgebra {
    case ast._Ident(Ident(TypeName("Null")))            => FreesF.TNull()
    case ast._Ident(Ident(TypeName("Double")))          => FreesF.TDouble()
    case ast._Ident(Ident(TypeName("Float")))           => FreesF.TFloat()
    case ast._Ident(Ident(TypeName("Int")))             => FreesF.TInt()
    case ast._Ident(Ident(TypeName("Long")))            => FreesF.TLong()
    case ast._Ident(Ident(TypeName("Boolean")))         => FreesF.TBoolean()
    case ast._Ident(Ident(TypeName("String")))          => FreesF.TString()
    case ast._Ident(x)                                  => FreesF.TNamedType(x.name.toString)
    case ast._SingletonTypeTree(x)                      => FreesF.TNamedType(x.ref.toString + ".type")
    case ast._AppliedTypeTree(AppliedTypeTree(x, tpes)) => FreesF.TGeneric(x, tpes)
    case ast._CaseClassDef(tree) if hasAnnotation("message")(tree) =>
      FreesF.TProduct(
        tree.name.toString,
        params.getOption(tree).toList.flatten.map { t =>
          FreesF.Field(t.name.toString, t.tpt)
        }
      )
  }

  def parseType[T](implicit T: Embed[FreesF, T]): Tree => T = scheme.ana(parseTypeAlgebra)

  def serTypeIso: Iso[SerializationType, ST] =
    Iso[SerializationType, ST] {
      case Protobuf       => ST.Protobuf
      case Avro           => ST.Avro
      case AvroWithSchema => ST.AvroWithSchema
    } {
      case ST.Protobuf       => Protobuf
      case ST.Avro           => Avro
      case ST.AvroWithSchema => AvroWithSchema
    }

  object ServiceClass {
    def unapply(tree: Tree): Option[(ClassDef, SerializationType)] =
      for {
        clazz             <- ast._ClassDef.getOption(tree)
        serviceAnnotation <- annotationsNamed("service").getAll(clazz).headOption
        serialization     <- serviceAnnotation.firstArg
      } yield
        (clazz, serialization.toString match {
          case "Protobuf"       => Protobuf
          case "Avro"           => Avro
          case "AvroWithSchema" => AvroWithSchema
        })
  }
}
