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
package fs2
package data

import csv.internals._

import cats._
import cats.data._
import cats.implicits._

package object csv {

  type DecoderResult[T] = Either[DecoderError, T]

  /** Transforms a stream of characters into a stream of CSV rows.
    */
  def rows[F[_]](separator: Char = ',')(
      implicit F: ApplicativeError[F, Throwable]): Pipe[F, Char, NonEmptyList[String]] =
    RowParser.pipe[F](separator)

  /** Transforms a stream of raw CSV rows into parsed CSV rows with headers. */
  def headers[F[_], Header](implicit F: ApplicativeError[F, Throwable],
                            Header: ParseableHeader[Header]): Pipe[F, NonEmptyList[String], CsvRow[Header]] =
    CsvRowParser.pipe[F, Header]

  /** Transforms a stream of raw CSV rows into CSV rows without headers (= without first line). Just returns the tail,
   * its value lies in the explicit name. */
  def skipHeaders[F[_]]: Pipe[F, NonEmptyList[String], NonEmptyList[String]] =
    _.tail

  def decode[F[_], R](implicit F: ApplicativeError[F, Throwable], R: RowDecoder[R]): Pipe[F, NonEmptyList[String], R] =
    _.evalMap(R(_).liftTo[F])

  def attemptDecode[F[_], R](implicit F: Applicative[F],
                             R: RowDecoder[R]): Pipe[F, NonEmptyList[String], DecoderResult[R]] =
    _.map(R(_))

  def decodeRow[F[_], Header, R](implicit F: ApplicativeError[F, Throwable],
                                 R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], R] =
    _.evalMap(R(_).liftTo[F])

  def attemptDecodeRow[F[_], Header, R](implicit F: Applicative[F],
                                        R: CsvRowDecoder[R, Header]): Pipe[F, CsvRow[Header], DecoderResult[R]] =
    _.map(R(_))

}
