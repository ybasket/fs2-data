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

import magnolia._
import fs2.data.csv._

import language.experimental.macros

trait DerivedCellEncoder[T] extends CellEncoder[T]

object DerivedCellEncoder {

  type Typeclass[T] = DerivedCellEncoder[T]

  def dispatch[T](ctx: SealedTrait[DerivedCellEncoder, T]): DerivedCellEncoder[T] =
    new DerivedCellEncoder[T] {
      def apply(value: T): String = ctx.dispatch(value)(s => s.typeclass.apply(s.cast(value)))
    }

  implicit def gen[T]: DerivedCellEncoder[T] = macro Magnolia.gen[T]

}

