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

import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

import freestyle.rpc.protocol.SerializationType
import freestyle.rpc.internal.util._

import qq.droste.Basis
import skeuomorph.freestyle.{FreesF, Protocol, Service}

trait IdlGenerator extends Generator {

  def serializationType: SerializationType
  def outputSubdir: String
  def fileExtension: String

  def inputFiles(files: Set[File]): Seq[File] =
    files.filter(_.getName.endsWith(ScalaFileExtension)).toSeq

  def generateFrom(
      inputFile: File,
      serializationType: String,
      options: String*): Option[(String, Seq[String])] = {
    val inputName = inputFile.getName.replaceAll(ScalaFileExtension, "")
    val definitions =
      ScalaParser.parse(
        Toolbox.parse(Files.readAllLines(Paths.get(inputFile.toURI)).asScala.mkString("\n")),
        inputName)

    generateFrom(definitions)
      .map(output => s"$outputSubdir/$inputName$fileExtension" -> output.split("\n").toList)
      .headOption
  }

  protected def generateFrom[T](proto: Protocol[T])(implicit T: Basis[FreesF, T]): Seq[String]

  private def filterServices[T](services: List[Service[T]]): List[Service[T]] =
    services
      .filter(_.serializationType == ScalaParser.serTypeIso.get(serializationType))
      .filter(_.operations.nonEmpty)
}
