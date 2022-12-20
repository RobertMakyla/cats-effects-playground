package my.playground

import cats.Parallel
import cats.effect.{IO, IOApp}

object Part5_IO_Parallelism extends IOApp.Simple {

  // IOs are usually sequential
  val mikeIO = IO(s"[${Thread.currentThread().getName}] Mike")
  val kateIO = IO(s"[${Thread.currentThread().getName}] Kate")

  val composedIO = for {
    a <- mikeIO
    b <- kateIO
  } yield s"$a and $b love Rock the JVM" // they both are run sequentially on the same thread

  // mapN extension method
  import cats.syntax.apply._
  val ioInt: IO[Int] = IO.delay(42)
  val ioString: IO[String] = IO.delay("Scala")
  val combinedIoIntAndString =
    (ioInt.debug, ioString.debug).mapN((num, string) => s"my goal in life is $num and $string") // also sequentially and on the same thread

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(ioInt.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(ioString.debug)

  import cats.effect.implicits._
  val parallelIO: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")

  // turn back to sequential
  val parallelIOBackToSeq: IO[String] = Parallel[IO].sequential(parallelIO) // evaluated on 3 threads, then results are back here in IO[String]

  // shorter parallel:
  import cats.syntax.parallel._
  val parallelIOBackToSeq_v2: IO[String] = (ioInt.debug, ioString.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  // failure in parallelism :
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))

  // compose success + failure
  val parallelWithFailure = (ioInt.debug, aFailure.debug).parMapN((num, string) => s"$num $string")

  // compose failure + failure
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  // the first effect to fail gives the failure of the result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(100)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)


  override def run: IO[Unit] =
    //composedIO.map(println)
//    combinedIoIntAndString.void
//    parallelIOBackToSeq.debug.void
//    parallelIOBackToSeq_v2.debug.void
    twoFailuresDelayed.debug.void
}