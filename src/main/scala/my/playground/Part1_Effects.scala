package my.playground

object Part1_Effects extends App {

  /**
   * In Functional Programming we rely on Referential Transparency -
   *
   * we can replace each expression with a value I returns
   * and we can be sure this time the value will also be the same - It won't change our app.
   *
   * Referential Transparency requires PURE FUNCTIONS ( no side effects )
   *
   *
   * However Side Effects are inevitable for useful apps (db changes, console printouts)
   *
   * EFFECTS - a bridge between pure functions and side-effects (it keeps Referential Transparency but allows Side effects)
   * it is:
   * - describing the VALUE with the signature
   * - describing the KIND OF CALCULATION with the signature
   * - when side effect is needed, effect construction is separate from effect execution
   *
   * eg. Option - is a correct Effect type
   * - Option[Int] tells us it will be an Int (if any)
   * - Option[Int] tells us it will be immediate (synchronous) calculation of Int or None.
   * - no side effect needed
   *
   * eg. Future - is NOT a correct Effect type
   * - Future[Int] tells us it will be an Int if successful
   * - Future tells us it's asynchronous
   * - Future fails here cause when we write Future(123) it's computed as soon as the thread is vacant
   */

  /**
   * let's try an IO
   */
  case class MyIO[A](unsafeRun: () => A) {

    //tip if you want to keep it lazy, start with MyIO( () => ...
    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
    //this has a correct type but the calculation is done immediately !! that's WRONG:   f(unsafeRun())

    def unit[B](b: B): MyIO[B] = MyIO(() => b)

    //every Monad is a functor
    def map[B](f: A => B): MyIO[B] = flatMap(a => unit(f(a)))
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm giving number TWO...")
    2
  })

  /* IO - is a correct Effect Type  (MOST GENERAL EFFECT TYPE - CAUSE IT MIGHT COMBINE ANY COMPUTATION WITH ANY SIDE EFFECTS)
   * - describes any computation that might produce side effects
   * - MyIO[A] calculates a value of type A, if it's successful
   * - side effects are required for the evaluation of () => A
   * --- and YES, the creation of MyIO does NOT produce the side effects on construction
   */

  val resultNotComputedYet: MyIO[Int] = for {
    a <- anIO
    b <- anIO
    c <- anIO
  } yield a + b + c

  println("Now I will see the side effects (not before): ")

  val six: Int = resultNotComputedYet.unsafeRun()

  println("result: " + six)


  /**
   * Exercises
   * 1 An IO which returns the current time of the system
   * 2 An IO which measures the duration of a computation (hint: use ex 1)
   * 3 An IO which prints something to the console
   * 4 An IO which reads a line (a string) from the std input
   */

  // 1
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  //teraz minuta 3:49 exercises
}
