package my.playground

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object Part3_IO_ErrorHandling {


  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE")) // lazy  - throws exception at unsafeRunSync()
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail")) // same result but more elegant - declaring explicitly instead of throwing/catching

  // handle exceptions

  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
    // add more cases
  }

  // turn IO[Int] into an IO[Either[Throwable, Int]]
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")

  // redeemWith - effectual redeem
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(ex => IO(println(s"FAIL: $ex")), value => IO(println(s"SUCCESS: $value")))

  /**
   * Exercises
   */

  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  //
  //    def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = ???
  //    def try2IO[A](aTry: Try[A]): IO[A] = ???
  //    def either2IO[A](anEither: Either[Throwable, A]): IO[A] = ???

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match {
      case Some(value) => IO.pure(value)
      case None => IO.raiseError(ifEmpty)
    }

  def try2IO[A](aTry: Try[A]): IO[A] =
    aTry match {
      case Success(value) => IO.pure(value)
      case Failure(ex) => IO.raiseError(ex)
    }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] =
    anEither match {
      case Right(value) => IO.pure(value)
      case Left(ex) => IO.raiseError(ex)
    }

  // 2 - handleError, handleErrorWith
  //
  //    def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = ???
  //    def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = ???

  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity)
//  io.handleError(handler)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
//  io.redeemWith(handler, IO.pure)
    io.handleErrorWith(handler)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

    dealWithIt.unsafeRunSync()
  }
}