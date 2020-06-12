/*
 * Copyright 2019 Lucas Satabin
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
package fs2.data.csv.magnolia

import cats.implicits._
import magnolia._
import fs2.data.csv._

import language.experimental.macros

trait DerivedCellDecoder[T] extends CellDecoder[T]

object DerivedCellDecoder  {
  type Typeclass[T] = DerivedCellDecoder[T]

  /*def combine[T](ctx: CaseClass[DerivedCellDecoder, T]): DerivedCellDecoder[T] = new DerivedCellDecoder[T] {
    def apply(cell: String): DecoderResult[T] = ctx.parameters.toList.traverse { p =>
      p.typeclass.apply(cell)
    }
  }*/

  def dispatch[T](ctx: SealedTrait[DerivedCellDecoder, T]): DerivedCellDecoder[T] =
    new DerivedCellDecoder[T] {
      def apply(cell: String): DecoderResult[T] = ctx.subtypes.find(_.typeName.short === cell).map(_.typeclass(cell)).liftTo[DecoderResult](new DecoderError(s"No value matching $cell found")).flatten
    }

  implicit def gen[T]: DerivedCellDecoder[T] = macro Magnolia.gen[T]
}
