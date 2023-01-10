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
    (ioInt.debug, ioString.debug).mapN((num, string) => s"my goal in life is $num and $string")

  /*
   mapN on sequential IOs
    - evaluates all IOs one by one on ONE Thread, then finally the result also in the same Thread
   */

  // Parallel IO (converted from sequential IO)
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(ioInt.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(ioString.debug)

  import cats.effect.implicits._
  val parallelIO: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")

  /*
   mapN on parallel IOs
   - evaluates all IOs at once in DIFFERENT Threads, then finally the result in yet another Thread
   */

  // turn back to sequential
  val parallelIOFromSeqIO: IO[String] = Parallel[IO].sequential(parallelIO) // // 3 different threads

  // shorter parallel:
  import cats.syntax.parallel._
  val parallelIOBuiltIn: IO[String] = (ioInt.debug, ioString.debug).parMapN((num, string) => s"my goal in life is $num and $string") // 3 different threads

  // failure in parallelism :
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))

  // compose success + failure - gives first failure
  val parallelWithFailure: IO[String] = (ioInt.debug, aFailure.debug).parMapN((num, string) => s"$num $string")

  // the first effect to fail gives the failure of the result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(100)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)


  override def run: IO[Unit] =
    //composedIO.map(println)
   // combinedIoIntAndString.debug.void
   // parallelIOFromSeqIO.debug.void
   // parallelIOBuiltIn.debug.void
   // parallelWithFailure.debug.void
      twoFailuresDelayed.debug.void
}