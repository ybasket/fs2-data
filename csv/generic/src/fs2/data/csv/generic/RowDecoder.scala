package fs2.data.csv.generic

import cats.MonadError
import cats.data.NonEmptyList
import cats.implicits._
import fs2.data.csv.CsvRow

import scala.annotation.tailrec

trait RowDecoder[V, Header] {

  def apply(row: CsvRow[Header]): Result[V]

}

object RowDecoder {
  def apply[V, Header](implicit ev: RowDecoder[V, Header]): RowDecoder[V, Header] = ev

  // Cats
  implicit def monadError[Header]: MonadError[RowDecoder[*, Header], DecodingException] = new MonadError[RowDecoder[*, Header], DecodingException] {
    override def pure[A](x: A): RowDecoder[A, Header] = successful[A, Header](x)

    override def flatMap[A, B](fa: RowDecoder[A, Header])(f: A => RowDecoder[B, Header]): RowDecoder[B, Header] =
      (row: CsvRow[Header]) => fa(row).flatMap(f(_)(row))

    override def tailRecM[A, B](a: A)(f: A => RowDecoder[Either[A, B], Header]): RowDecoder[B, Header] = new RowDecoder[B, Header] {
      @tailrec
      private[this] def step(row: CsvRow[Header], a1: A): Result[B] = f(a1)(row) match {
        case l @ Left(_)     => l.asInstanceOf[Result[B]]
        case Right(Left(a2)) => step(row, a2)
        case Right(Right(b)) => Right(b)
      }

      final def apply(row: CsvRow[Header]): Result[B] = step(row, a)
    }

    override def raiseError[A](e: DecodingException): RowDecoder[A, Header] = failed(e)

    override def handleErrorWith[A](fa: RowDecoder[A, Header])(f: DecodingException => RowDecoder[A, Header]): RowDecoder[A, Header] =
      (row: CsvRow[Header]) => fa(row).leftFlatMap(f(_)(row))
  }

  // Helpers
  def successful[V, Header](v: V): RowDecoder[V, Header] = _ => v.asRight
  def failed[V, Header](e: DecodingException): RowDecoder[V, Header] = _ => e.asLeft

  // Instances
  implicit def nonEmptyStringListRowDecoder[Header]: RowDecoder[NonEmptyList[String], Header] = _.values.asRight
  implicit def nonEmptyListRowDecoder[Header, V](implicit ev: CellDecoder[V]): RowDecoder[NonEmptyList[V], Header] = _.values.traverse(ev.apply)
  implicit def headerStringMapRowDecoder[Header]: RowDecoder[Map[Header, String], Header] = _.toMap.toRight(RowDecodingException("No headers given"))
  implicit def headerMapRowDecoder[Header, V](implicit ev: CellDecoder[V]): RowDecoder[Map[Header, V], Header] = row =>
    row.headers.toRight(RowDecodingException("No headers given"))
      .flatMap(h => row.values.zipWith(h)(ev(_) tupleLeft _).sequence.map(_.toList.toMap))

}
