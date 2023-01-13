package my.playground

import cats.effect.kernel.Outcome._
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * FIBER - a basic class of IO concurrency - Incredibly lightweight
 *
 * FIBERS vs THREADS:
 * Threads - are active entities, they can run code
 * Fibers - are passive entities - a description of an effect - managed by Cats Effect runtime
 *
 * Cats Effects manages let's say at most 100 threads on 1 machine (more is too heavy) (measured by number of Threads per CPU)
 * In the contrast we can have hundreds of millions of fibers (measured in GB of heap)
 *
 * What FIBERS give us:
 * - no need to play with threads / locks
 * - Cats Effects gives us Thread management for free
 * - no Future.callbacks methods which are asynchronous and hard to control/predict/test
 * - we stay in pure functional world
 *
 * advanced :
 * - BLOCKING/SLEEPING an IO in a fiber is actually DESCHEDULING an effect from a thread (no JVM thread is ever blocked) - it is called SEMANTIC BLOCKING
 * -a fiber can run on many JVM threads (eg. between sleeping)
 *
 */
object Part7_Fibers extends IOApp.Simple {

  val ioInt = IO.pure(42)
  val ioStr = IO.pure("Scala")

  def sameThreadIOs(): IO[Unit] = for {
    _ <- ioInt.debug
    _ <- ioStr.debug
  } yield ()

  // introducing Fiber: a data structure describing an effect running on some thread
  // types: IO - effect type that will run on Thread managed by cats effect
  //        Throwable - possible error type
  //        String - returned type in case of success
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  def differentThreadIOs() = for {
    fib <- ioInt.debug.start // IO[A].start is an effectful operation ...
                            // ... it returns IO[Fiber[IO, Throwable, A]] running on separate thread
    _ <- ioStr.debug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    outcome <- fib.join // Fiber[IO, Throwable, A].join is an effectful operation ...
                        // ... it returns IO[Outcome[IO, Throwable, A]] and we go back to main thread
  } yield outcome

  /*
    possible outcomes:
    - success with an IO
    - failure with an exception
    - cancelled
   */

  val someIOOnAnotherThread: IO[Outcome[IO, Throwable, Int]] = runOnSomeOtherThread(ioInt)

  val someResultFromAnotherThread: IO[Int] = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(-1)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task: IO[String] = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    // onCancel is a "finalizer", allowing you to free up resources in case you get canceled
    val taskWithCancellationHandler = task.onCancel(IO("someone is cancelling me :(").debug.void) //  IO.onCancel
    // - will be returned in case of cancelling fiber with this IO

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("I am about to cancel").debug // running on the calling thread
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /**
   * Exercises:
   *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   */

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val myOutcome = for {
      fib <- io.start
      outcome <- fib.join
    } yield outcome

    myOutcome flatMap {
      case Succeeded(effect) => effect
      case Canceled() => IO.raiseError(new RuntimeException("it's Canceled."))
      case Errored(e) => IO.raiseError(new RuntimeException("it's Errored: " + e.getMessage))
    }
  }

  def testEx1() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done!").debug >> IO(42)
    processResultsFromFiber(aComputation).void
  }

  /**
   * Exercises:
   * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   *    - if the first IO doesn't error but second IO returns an error, raise that error
   *    - otherwise (if either of them is cancelled), raise a RuntimeException
   *
   */
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val myOutcomes: IO[(Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])] = for {
      fibA <- ioa.start
      fibB <- iob.start
      outA <- fibA.join
      outB <- fibB.join
    } yield (outA, outB)

    myOutcomes.flatMap {
      case (Succeeded(aEffect: IO[A]), Succeeded(bEffect: IO[B])) => for {
        a <- aEffect
        b <- bEffect
      } yield (a, b)
      case (Errored(aErr), Succeeded(_)) => IO.raiseError(aErr)
      case (Succeeded(_), Errored(bErr)) => IO.raiseError(bErr)
      case _ => IO.raiseError(new RuntimeException("Some computation canceled."))
    }

  }

  def testEx2() = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug
    tupleIOs(firstIO, secondIO).debug.void
  }

  /**
   * Exercises:
   * 3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes, then the fiber is canceled
   *    - the method returns an IO[A] which contains
   *      - the original value if the computation is successful before the timeout signal
   *      - the exception if the computation is failed before the timeout signal
   *      - a RuntimeException if it times out (i.e. cancelled by the timeout) */

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- (IO.sleep(duration) >> fib.cancel).start // careful - fibers can leak if we start a heavy fiber and don't do anything with the result
      outcome <- fib.join
    } yield outcome

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  def testEx3() = {
    val aComputation: IO[Int] = IO("starting").debug >> IO.sleep(1.second) >> IO("done!").debug >> IO(42)
    timeout(aComputation, 500.millis).debug.void
  }

  override def run =
  //testCancel().debug.void
  //testEx1()
  //testEx2()
  testEx3()

}