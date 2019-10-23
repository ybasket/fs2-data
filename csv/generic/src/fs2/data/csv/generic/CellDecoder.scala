package fs2.data.csv.generic

import cats.MonadError
import cats.implicits._
import scala.annotation.tailrec

trait CellDecoder[V] {

  def apply(content: String): Result[V]

}

object CellDecoder {
  def apply[V](implicit ev: CellDecoder[V]): CellDecoder[V] = ev

  // Cats
  implicit def monadError[Header]: MonadError[CellDecoder[*], DecodingException] = new MonadError[CellDecoder[*], DecodingException] {
    override def pure[A](x: A): CellDecoder[A] = successful[A](x)

    override def flatMap[A, B](fa: CellDecoder[A])(f: A => CellDecoder[B]): CellDecoder[B] =
      (cell: String) => fa(cell).flatMap(f(_)(cell))

    override def tailRecM[A, B](a: A)(f: A => CellDecoder[Either[A, B]]): CellDecoder[B] = new CellDecoder[B] {
      @tailrec
      private[this] def step(cell: String, a1: A): Result[B] = f(a1)(cell) match {
        case l @ Left(_)     => l.asInstanceOf[Result[B]]
        case Right(Left(a2)) => step(cell, a2)
        case Right(Right(b)) => Right(b)
      }

      final def apply(cell: String): Result[B] = step(cell, a)
    }

    override def raiseError[A](e: DecodingException): CellDecoder[A] = failed(e)

    override def handleErrorWith[A](fa: CellDecoder[A])(f: DecodingException => CellDecoder[A]): CellDecoder[A] =
      (cell: String) => fa(cell).leftFlatMap(f(_)(cell))
  }

  // Helpers
  def successful[V](v: V): CellDecoder[V] = _ => v.asRight
  def failed[V](e: DecodingException): CellDecoder[V] = _ => e.asLeft

  // Instances
  implicit val stringCellDecoder: CellDecoder[String] = _.asRight
  implicit val booleanCellDecoder: CellDecoder[Boolean] = catchingException("Not a valid boolean")(_.toBoolean)
  implicit val longCellDecoder: CellDecoder[Long] = catchingException("Not a valid long")(_.toLong)
  implicit val intCellDecoder: CellDecoder[Int] = catchingException("Not a valid int")(_.toInt)
  implicit val shortCellDecoder: CellDecoder[Short] = catchingException("Not a valid short")(_.toShort)
  implicit val byteCellDecoder: CellDecoder[Byte] = catchingException("Not a valid byte")(_.toByte)
  implicit val doubleCellDecoder: CellDecoder[Double] = catchingException("Not a valid double")(_.toDouble)
  implicit val floatCellDecoder: CellDecoder[Float] = catchingException("Not a valid float")(_.toFloat)
  implicit val bigIntCellDecoder: CellDecoder[BigInt] = catchingException("Not a valid big int")(BigInt.apply)
  implicit val bigDecimalCellDecoder: CellDecoder[BigDecimal] = catchingException("Not a valid big decimal")(BigDecimal.apply)

  implicit def optionCellDecoder[V](implicit ev: CellDecoder[V]): CellDecoder[Option[V]] = cell => {
    if(cell.nonEmpty) {
      ev(cell).map(_.some)
    } else {
      None.asRight
    }
  }

  implicit def resultDecoder[V](implicit ev: CellDecoder[V]): CellDecoder[Result[V]] = ev.map(_.asRight)

  private def catchingException[V](msg: String)(parse: String => V): CellDecoder[V] =
    cell => Either.catchNonFatal(parse(cell)).leftMap(t => RowDecodingException(msg, t.some))
}
