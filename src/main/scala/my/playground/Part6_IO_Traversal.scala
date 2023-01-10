package my.playground

import cats.effect.{IO, IOApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

/*
  cats.Traverse is perfect for converting List[Future[T]] into one result Future[List[T]]

  TRAVERSE - allows double nested data structures to be defined inside out + mapping
  SEQUENCE - allows double nested data structures to be defined inside out but that's it ! - no mapping
 */

object Part6_IO_Traversal extends IOApp.Simple {


  ///////////////////// Traverse for Futures

  def string2LengthFut(string: String): Future[Int] = Future {
    Thread.sleep(1000)
    string.split(" ").length
  }

  val workLoad: List[String] = List("I quite like CatsEffects", "Scala is great", "looking forward to some awesome stuff")

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(string2LengthFut)
    // Future[List[Int]] would be hard to obtain - we could use Traverse
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._ // type classes, like: Traverse[List]

  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    // traverse
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(string2LengthFut)
    // ^^ this stores ALL the results
    singleFuture.foreach(println)
  }

  ///////////////////// Traverse for IO

  // sequential traversal

  def string2LengthIO(string: String): IO[Int] = IO {
    Thread.sleep(1000)
    string.split(" ").length
  }.debug

  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(string2LengthIO) // sequential traverse - 1 Thread (one by one)

  // parallel traversal

  import cats.syntax.parallel._ // parTraverse extension method

  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(string2LengthIO)

  /**
   *  Exercises
   */

  // hint: use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(x => x)

  // hard version - any Traverse[F] - not just Traverse[List]
  def sequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(x => x)

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(x => x)

  // hard version
  def parSequence_v2[F[_] : Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(x => x)

  // Sequence API
  val ios: List[IO[Int]] = workLoad.map(string2LengthIO)
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)  // === traverse(ios)(x => x)

  // parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence // extension method from the Parallel syntax package

  override def run =
//    singleIO.map(_.sum).debug.void
//    parallelSingleIO.map(_.sum).debug.void
      parallelSingleIO_v3.map(_.sum).debug.void
}