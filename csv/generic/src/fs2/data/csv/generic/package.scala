package fs2.data.csv

import fs2.{Pipe, RaiseThrowable}

import scala.language.higherKinds

package object generic {

  sealed trait DecodingException extends Exception
  case class RowDecodingException(message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull) with DecodingException
  case class CellDecodingException(message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull) with DecodingException

  type Result[V] = Either[DecodingException, V]
  type AccumulatedResult[V] = Either[DecodingException, V]

  def decodeRows[F[_] : RaiseThrowable, V, Header](implicit decoder: RowDecoder[V, Header]): Pipe[F, CsvRow[Header], V] =
    _.map(decoder.apply).rethrow

}
