package my.playground

import cats.effect.IO


import scala.io.StdIn

object Part2_IO {

  // IO pure vs delay
  val ioInt: IO[Int] = IO.pure(42) // PURE - argument should NOT have side effects, cause it's EVALUATED EAGERLY !
  val aDelayedIO: IO[Int] = IO.delay {         // DELAY - EVALUATED LAZILY (using call-by-name)
    println("I'm producing an integer")
    54
  }

  // IO.delay() == IO.apply()
  val aDelayedIO_v2: IO[Int] = IO {
    println("I'm producing an integer")
    54
  }

  // map, flatMap
  val ioMapped: IO[Int] = ioInt.map(_ * 2)
  val ioFlatMapped: IO[Unit] = ioInt.flatMap(x => IO.delay(println(x)))

  def greeting(): IO[Unit] = for {
    _    <- IO(println("what's your name?"))
    name <- IO(StdIn.readLine())
    _    <- IO.delay(println(s"Hello ${name} !!!"))
  } yield ()


  // COMBINING EFFECTS: mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val ioCombined: IO[Int] = (ioInt, ioMapped).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // platform of Cats Effects
    greeting.unsafeRunSync()
  }

  /**
   * Exercises
   */

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  //
  //   def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ???


  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  //
  //   def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ???


  // 3 - repeat an IO effect forever
  // hint: use flatMap + recursion
  //
  //   def forever[A](io: IO[A]): IO[A] = ???


  // 4 - convert an IO to a different type
  // hint: use map
  //   def convert[A, B](ioa: IO[A], value: B): IO[B] = ???


  // 5 - discard value inside an IO, just return Unit
  //    def asUnit[A](ioa: IO[A]): IO[Unit] = ???


  // 6 - fix stack recursion - make it tail recursion
  //   old way: def sum(n: Int): Int = if (n <= 0) 0 else n + sum(n - 1)
  //
  //   def sumIO(n: Int): IO[Int] = ???


  // 7 (hard) - write a fibonacci IO that does NOT crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatMap heavily
  //
  //   def fibonacci(n: Int): IO[BigInt] = ???


}
