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

  //teraz IO Exercise https://rockthejvm.com/courses/1479486/lectures/33924538
}
