package my

import cats.effect.IO

package object playground {

  implicit class DebugWrapper[A](io: IO[A]){

    def debug: IO[A] = for {
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    } yield a

  }
}
