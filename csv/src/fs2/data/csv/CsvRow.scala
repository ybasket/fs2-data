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
package fs2.data.csv

import cats.data._
import cats.implicits._

case class CsvRow[Header](values: NonEmptyList[String], headers: NonEmptyList[Header]) {

  private lazy val byHeader: Map[Header, String] =
    headers.toList.zip(values.toList).toMap

  /** Number of cells in the row. */
  def size: Int = values.size

  /** Returns the content of the cell at `idx` if it exists.
    * Returns `None` if `idx` is out of row bounds.
    * An empty cell value results in `Some("")`.
    */
  def apply(idx: Int): Option[String] =
    values.get(idx)

  /** Modifies the cell content at the given `idx` using the function `f`.
    */
  def modify(idx: Int)(f: String => String): CsvRow[Header] =
    if (idx < 0 || idx >= values.size)
      this
    else
      new CsvRow(values.zipWithIndex.map {
        case (cell, i) =>
          if (i === idx)
            f(cell)
          else
            cell
      }, headers)

  /** Modifies the cell content at the given `header` using the function `f`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def modify(header: Header)(f: String => String): CsvRow[Header] =
    modify(headers.toList.indexOf(header))(f)

  /** Returns the row with the cell at `idx` modifed to `value`. */
  def updated(idx: Int, value: String): CsvRow[Header] =
    if (idx < 0 || idx >= values.size)
      this
    else
      new CsvRow(values.zipWithIndex.map {
        case (cell, i) =>
          if (i === idx)
            value
          else
            cell
      }, headers)

  /** Returns the row with the cell at `header` modifed to `value`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be modified. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def updated(header: Header, value: String): CsvRow[Header] =
    updated(headers.toList.indexOf(header), value)

  /** Returns the row without the cell at the given `idx`.
    * If the resulting row is empty, returns `None`.
    */
  def delete(idx: Int): Option[CsvRow[Header]] =
    if (idx < 0 || idx >= values.size) {
      Some(this)
    } else {
      val (before, after) = values.toList.splitAt(idx)
      val (h1, h2) = headers.toList.splitAt(idx)
      val newHeaders = h1 ++ h2.tail
      (NonEmptyList.fromList(before ++ after.tail), NonEmptyList.fromList(newHeaders)).mapN(new CsvRow(_, _))
    }

  /** Returns the row without the cell at the given `header`.
    * If the resulting row is empty, returns `None`.
    *
    * **Note:** Only the first occurrence of the values with the given header
    * will be deleted. It shouldn't be a problem in the general case as headers
    * should not be duplicated.
    */
  def delete(header: Header): Option[CsvRow[Header]] =
    delete(headers.toList.indexOf(header))

  /** Returns the content of the cell at `headers` if it exists.
    * Returns `None` if `header` does not exist for the row.
    * An empty cell value results in `Some("")`.
    */
  def apply(header: Header): Option[String] =
    byHeader.get(header)

  /** Returns a map representation of this row if headers are defined, otherwise
    * returns `None`.
    */
  def toMap: Map[Header, String] =
    byHeader

}

object CsvRow {

  def fromList[Header](l: List[(Header, String)]): Option[CsvRow[Header]] = {
    val (hs, vs) = l.unzip
    (NonEmptyList.fromList(vs), NonEmptyList.fromList(hs)).mapN(new CsvRow(_, _))
  }

  def fromNel[Header](nel: NonEmptyList[(Header, String)]): CsvRow[Header] = {
    val (hs, vs) = nel.toList.unzip
    new CsvRow(NonEmptyList.fromListUnsafe(vs), NonEmptyList.fromListUnsafe(hs))
  }

}
