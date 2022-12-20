package my.playground

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn
import Part4_IO_Apps._

object Part4_IO_Apps {

  val program: IO[Unit] = for {
    _ <- IO(println("what's your name ?"))
    name <- IO(StdIn.readLine())
    _ <- IO(println("hello, " + name))
  } yield ()

}

object ClassicApp {

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

    program.unsafeRunSync()
  }
}

object MyIOApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    //program.map(_ => ExitCode.Success)
    program.as(ExitCode.Success)
}

object MyIOAppSimple extends IOApp.Simple {
  override def run: IO[Unit] = program
}