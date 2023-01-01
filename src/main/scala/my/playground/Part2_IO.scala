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

  /**
   * Exercise: write a one-liner program that reads 2 lines of strings (one by one), combines the strings and then prints it out.
   */
  def smallProgram_v2(): IO[Unit] =
    (IO(scala.io.StdIn.readLine()), IO(scala.io.StdIn.readLine())).mapN(_ + _).map(println)

  /**
   * Exercises
   */

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  //
  //   def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ???

  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    // ioa *> iob  // 'andThen' called by value - NOT suitable for recursion.
    //            // '*>' is not running the IO (unsafeRun) but it is evaluating the effect immediately, so when it's in recursion it will give
    //ioa >> iob // 'andThen' called by name (lazy) - suitable for recursion.
    //          // '>>' is not even evaluating the effect (not touching it)
      ioa.flatMap(_ => iob) // flatMap is also EVALUATED LAZILY - just like '>>' - suitable for recursion
  }

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  //
  //   def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ???

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob // Like *>, but keeps the result of the source.

  // 3 - repeat an IO effect forever
  // hint: use flatMap + recursion
  //
  //   def forever[A](io: IO[A]): IO[A] = ???

  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io)) // '_' because we don't care about the result of the io, we just keep running it.

  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io) // OK cause '>>' is evaluated lazily - suitable for recursion

  def forever_stack_overflow[A](io: IO[A]): IO[A] =
    io *> forever_stack_overflow(io) // we get Stack Overflow
    // '*>' runs the io immediately and enters the infinite loop even before it has a chance to be launched by unsafeRunSync()

  def foreverM[A](io: IO[A]): IO[A] =
      io.foreverM // with tail recursion

  // 4 - convert an IO to a different type
  // hint: use map
  //   def convert[A, B](ioa: IO[A], value: B): IO[B] = ???

  def convert[A, B](ioa: IO[A], b: B): IO[B] =
    //ioa.map(_ => b)
    ioa.as(b) // same as map(_ -> b)

  // 5 - discard value inside an IO, just return Unit
  //    def asUnit[A](ioa: IO[A]): IO[Unit] = ???

  def asUnit[A](ioa: IO[A]): IO[Unit] = {
    //ioa.map(_ => ())
    //ioa.as(()) // very ugly but working
    ioa.void // most elegant
  }

  // 6 - fix stack recursion - so that it doesn't crash with big numbers (like tail rec, in original scala)
  //   old way: def sum(n: Int): Int = if (n <= 0) 0 else n + sum(n - 1)
  //
  //   def sumIO(n: Int): IO[Int] = ???

  def sum_stack_overflow(n: Int): Int = if (n <= 0) 0 else n + sum_stack_overflow(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0)
      IO.pure(0)
    else
      for { // flatMap is STACK-SAFE so it will not give stack overflow
        theN <- IO.pure(n)
        sum <- sumIO(n - 1)
      } yield theN + sum

  // 7 (hard) - write a fibonacci IO that does NOT crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatMap heavily
  //
  //   def fibonacci(n: Int): IO[BigInt] = ???

  def fibonacci_flatMap(n: Int): IO[BigInt] =
    if (n < 2)
      IO.pure(1)
    else
      for {
        fibMinus1 <- fibonacci_flatMap(n - 1)
        fibMinus2 <- fibonacci_flatMap(n - 2)
      } yield fibMinus1 + fibMinus2

  def fibonacci_defer(n: Int): IO[BigInt] =
    if (n < 2)
      IO.pure(1)
    else
      for {
        fibMinus1 <- IO.defer(fibonacci_defer(n - 1))
        fibMinus2 <- IO.defer(fibonacci_defer(n - 2))
      } yield fibMinus1 + fibMinus2

  /*
     Fun Fact - we can skip IO.defer and use flatMap in for-comprehension :
       fibMinus1 <- fibonacci_flatMap(n-1)
     and still don't get the stack overflow, because flatMap is also suspending lazily the effect.
  */

  /**
   * To Sum-up:
   * def apply[A](a: => A): IO[A]  Suspends (in IO) a synchronous side effect which produces value A.
   * def delay[A](a: => A): IO[A]  exactly the same. It's all just LAZY SUSPENDING OF AN EFFECT (call by name)
   *
   * IO.defer[A](ioa: => IO[A]): IO[A]   Suspends (in IO) a synchronous side effect which produces an IO[A].
   *                                     It's like IO.delay(..).flatten
   */

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // platform of Cats Effects

    //    greeting.unsafeRunSync()
    //    forever(IO {println("forever with flatMap"); Thread.sleep(123)}).unsafeRunSync()
    //    forever_v2(IO {println("forever with >> "); Thread.sleep(123)}).unsafeRunSync()
    //    forever_stack_overflow(IO {println("forever_stack_overflow"); Thread.sleep(123)}).unsafeRunSync()
    //    foreverM(IO {println("foreverM"); Thread.sleep(123)}).unsafeRunSync()

//        sum_stack_overflow(20000)
    //    println(sumIO(20000).unsafeRunSync())

//    (1 to 33).foreach { i => println(i + ", fibonacci with IO.defer = " + fibonacci_defer(i).unsafeRunSync()) }
//    (1 to 33).foreach { i => println(i + ", fibonacci with flatMap  = " + fibonacci_flatMap(i).unsafeRunSync()) }
  }
}
