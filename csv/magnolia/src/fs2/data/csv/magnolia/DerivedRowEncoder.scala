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

import cats.data.NonEmptyList
import cats.implicits._
import magnolia._
import fs2.data.csv._

trait DerivedRowEncoder[T] extends RowEncoder[T]

object DerivedRowEncoder {

  type Typeclass[T] = DerivedRowEncoder[T]

  def combine[T](ctx: CaseClass[DerivedRowEncoder, T]): DerivedRowEncoder[T] = new DerivedRowEncoder[T] {
    def apply(value: T): NonEmptyList[String] = ctx.parameters.toList.toNel.nested.flatMap { p =>
      p.typeclass.apply(p.dereference(value))
    }.value
  }

  def dispatch[T](ctx: SealedTrait[DerivedRowEncoder, T]): DerivedRowEncoder[T] =
    new DerivedRowEncoder[T] {
      def apply(value: T): String = ctx.dispatch(value)(s => s.typeclass.apply(s.cast(value)))
    }

  implicit def gen[T]: DerivedRowEncoder[T] = macro Magnolia.gen[T]

}
