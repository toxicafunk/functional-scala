// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.effects

import java.io.IOException

import jdk.nashorn.internal.ir.RuntimeNode.Request
import scalaz.zio._
import scalaz.zio.console._

import scala.concurrent.duration._

object zio_background {
  sealed trait Program[A] { self =>
    final def *> [B](that: Program[B]): Program[B] =
      self.flatMap(_ => that)
    final def <* [B](that: Program[B]): Program[A] =
      self.flatMap(a => that.map(_ => a))

    final def map[B](f: A => B): Program[B] =
      self match {
        case Program.ReadLine(next) =>
          Program.ReadLine(input => next(input).map(f))
        case Program.WriteLine(line, next) =>
          Program.WriteLine(line, next.map(f))
        case Program.Return(value) =>
          Program.Return(() => f(value()))
      }

    final def flatMap[B](f: A => Program[B]): Program[B] =
      self match {
        case Program.ReadLine(next) =>
          Program.ReadLine(input => next(input).flatMap(f))
        case Program.WriteLine(line, next) =>
          Program.WriteLine(line, next.flatMap(f))
        case Program.Return(value) =>
          f(value())
      }
  }
  object Program {
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class Return[A](value: () => A) extends Program[A]

    val readLine: Program[String] = ReadLine(point[String](_))
    def writeLine(line: String): Program[Unit] = WriteLine(line, point(()))
    def point[A](a: => A): Program[A] = Return(() => a)
  }

  import Program.{readLine, writeLine, point}

  val yourName1: Program[Unit] =
    writeLine("What is your name?").flatMap(_ =>
      readLine.flatMap(name =>
        writeLine("Hello, " + name + ", good to meet you!").flatMap(_ =>
          point(())
        )
      )
    )

  //
  // EXERCISE 1
  //
  // Rewrite `program1` to use a for comprehension.
  //
  val yourName2: Program[Unit] = ???

  //
  // EXERCISE 2
  //
  // Rewrite `yourName2` using the helper function `getName`, which shows how
  // to create larger programs from smaller programs.
  //
  val yourName3: Program[Unit] = ???

  val getName: Program[String] =
    writeLine("What is your name?").flatMap(_ => readLine)

  //
  // EXERCISE 3
  //
  // Implement the following effectful procedure, which interprets
  // `Program[A]` into `A`. You can use this procedure to "run" programs.
  //
  def interpret[A](program: Program[A]): A =
    ???

  //
  // EXERCISE 4
  //
  // Implement the following function, which shows how to write a combinator
  // that operates on programs.
  //
  def sequence[A](programs: List[Program[A]]): Program[List[A]] =
    ???

  //
  // EXERCISE 5
  //
  // Translate the following procedural program into a purely functional program
  // using `Program` and a for comprehension.
  //
  def ageExplainer1(): Unit = {
    println("What is your age?")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case Some(age) =>
        if (age < 12) println("You are a kid")
        else if (age < 20) println("You are a teenager")
        else if (age < 30) println("You are a grownup")
        else if (age < 50) println("You are an adult")
        else if (age < 80) println("You are a mature adult")
        else if (age < 100) println("You are elderly")
        else println("You are probably lying.")
      case None =>
        println("That's not an age, try again")

        ageExplainer1()
    }
  }
  def ageExplainer2: Program[Unit] = ???
}

object zio_type {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Write the type of `IO` values that can fail with an `Exception`, or
  // may produce an `A`.
  //
  type Exceptional[A] = IO[???, ???]

  //
  // EXERCISE 2
  //
  // Write the type of `IO` values that can fail with a `Throwable`, or
  // may produce an `A`.
  //
  type Task[A] = IO[???, ???]

  //
  // EXERCISE 3
  //
  // Write the type of `IO` values that cannot fail, but may produce an `A.`
  //
  type Infallible[A] = IO[???, ???]

  //
  // EXERCISE 4
  //
  // Write the type of `IO` values that cannot produce a value, but may fail
  // with an `E`.
  //
  type Unproductive[E] = IO[???, ???]

  //
  // EXERCISE 5
  //
  // Write the type of `IO` values that cannot fail or produce a value.
  //
  type Unending = IO[???, ???]
}

object zio_values {
  //
  // EXERCISE 1
  //
  // Using the `IO.now` method, lift the integer `2` into a strictly-evaluated
  // `IO`.
  //
  val ioInteger: IO[Nothing, Int] = ???

  //
  // EXERCISE 2
  //
  // Using the `IO.point` method, lift the string "Functional Scala" into a
  // lazily-evaluated `IO`.
  //
  val ioString: IO[Nothing, String] = ???

  //
  // EXERCISE 3
  //
  // Using the `IO.fail` method to lift the string "Bad Input" into a failed
  // `IO`.
  //
  val failedInput: IO[String, Nothing] = ???
}

object zio_composition {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Map the `IO[Nothing, Int]` into an `IO[Nothing, String]` by converting the
  // integer into its string rendering using the `map` method of the `IO`
  // object.
  //
  (IO.point(42) ? : IO[Nothing, String])

  //
  // EXERCISE 2
  //
  // Map the `IO[Int, Nothing]` into an `IO[String, Nothing]` by converting the
  // integer error into its string rendering using the `leftMap` method of the
  // `IO` object.
  //
  (IO.fail(42) ? : IO[String, Nothing])

  //
  // EXERCISE 3
  //
  // Using the `flatMap` and `map` methods of `IO`, add `ioX` and `ioY`
  // together.
  //
  val ioX: IO[Nothing, Int] = IO.point(42)
  val ioY: IO[Nothing, Int] = IO.point(58)
  val ioXPlusY: IO[Nothing, Int] = ioX.flatMap(???)

  //
  // EXERCISE 4
  //
  // Using the `flatMap` method of `IO`, implement `ifThenElse`.
  //
  def ifThenElse[E, A](bool: IO[E, Boolean])(
    ifTrue: IO[E, A], ifFalse: IO[E, A]): IO[E, A] = ???
  val exampleIf = ifThenElse(IO.point(true))(IO.point("It's true!"), IO.point("It's false!"))

  //
  // EXERCISE 5
  //
  // Translate the following program, which uses for-comprehensions, to its
  // equivalent chain of `flatMap`'s, followed by a final `map`.
  //
  for {
    v1 <- IO.point(42)
    v2 <- IO.point(58)
  } yield "The total is: " + (v1 + v2).toString

  //
  // EXERCISE 6
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def decode1(read: () => Byte): Either[Byte, Int] = {
    val b = read()
    if (b < 0) Left(b)
    else {
      Right(b.toInt +
      (read().toInt << 8) +
      (read().toInt << 16) +
      (read().toInt << 24))
    }
  }
  def decode2[E](read: IO[E, Byte]): IO[E, Either[Byte, Int]] =
    ???

  //
  // EXERCISE 7
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def getName1(print: String => Unit, read: () => String): Option[String] = {
    print("Do you want to enter your name?")
    read().toLowerCase.take(1) match {
      case "y" => Some(read())
      case _ => None
    }
  }
  def getName2[E](print: String => IO[E, String], read: IO[E, String]): IO[E, Option[String]] = ???

  //
  // EXERCISE 8
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def forever1(action: () => Unit): Unit =
    while (true) action()
  def forever2[A](action: IO[Nothing, A]): IO[Nothing, Nothing] =
    ???

  //
  // EXERCISE 9
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def repeatN1(n: Int, action: () => Unit): Unit =
    if (n <= 0) ()
    else {
      action()
      repeatN1(n - 1, action)
    }
  def repeatN2[E](n: Int, action: IO[E, Unit]): IO[E, Unit] =
    ???

  //
  // EXERCISE 10
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) *> IO.point(19)

  //
  // EXERCISE 11
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) <* IO.point(19)

  //
  // EXERCISE 12
  //
  // Translate the following expression into an equivalent expression using
  // the `map` and `flatMap` methods of the `IO` object.
  //
  (IO.point(42) <* IO.point(19)) *> IO.point(1)
}

object zio_failure {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.fail` method, create an `IO[String, Int]` value that
  // represents a failure with a string error message, containing a user-
  // readable description of the failure.
  //
  val stringFailure1: IO[String, Int] = ???

  //
  // EXERCISE 2
  //
  // Using the `IO.fail` method, create an `IO[Int, String]` value that
  // represents a failure with an integer error code.
  //
  val intFailure: IO[Int, String] = ???

  //
  // EXERCISE 3
  //
  // Transform the error of `intFailure` into its string representation using
  // the `leftMap` method of `IO`.
  //
  val stringFailure2: IO[String, String] = ???

  //
  // EXERCISE 4
  //
  // Translate the following exception-throwing program into its ZIO equivalent.
  //
  def accessArr1[A](i: Int, a: Array[A]): A =
    if (i < 0 || i >= a.length) throw new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")")
    else a(i)
  def accessArr2[A](i: Int, a: Array[A]): IO[IndexOutOfBoundsException, A] =
    ???

  //
  // EXERCISE 5
  //
  // Translate the following ZIO program into its exception-throwing equivalent.
  //
  trait DenomIsZero
  object DenomIsZero extends DenomIsZero {}
  def divide1(n: Int, d: Int): IO[DenomIsZero, Int] =
    if (d == 0) IO.fail(DenomIsZero)
    else IO.now(n / d)
  def divide2(n: Int, d: Int): Int = ???

  //
  // EXERCISE 6
  //
  // Recover from a division by zero error by returning `-1`.
  //
  val recovered1: IO[Nothing, Int] =
    divide1(100, 0).attempt.map {
      case Left(error) => ???
      case Right(value) => ???
    }

  //
  // EXERCISE 7
  //
  // Recover from a division by zero error by using `redeem`.
  //
  val recovered2: IO[Nothing, Int] =
    divide1(100, 0).redeem(???, ???)

  //
  // EXERCISE 8
  //
  // Use the `orElse` method of `IO` to try `firstChoice`, and fallback to
  // `secondChoice` only if `firstChoice` fails.
  //
  val firstChoice: IO[DenomIsZero, Int] = divide1(100, 0)
  val secondChoice: IO[Nothing, Int] = IO.now(400)
  val combined: IO[Nothing, Int] = ???
}

object zio_effects {
  import scala.io.StdIn.readLine
  import scala.io.Source
  import java.io.File
  import java.util.concurrent.{Executors, TimeUnit}

  type ??? = Nothing
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.sync` method, wrap Scala's `println` method to import it into
  // the world of pure functional programming.
  //
  def putStrLn(line: String): IO[Nothing, Unit] = println ?

  //
  // EXERCISE 2
  //
  // Using the `IO.sync` method, wrap Scala's `readLine` method to import it
  // into the world of pure functional programming.
  //
  val getStrLn: IO[Nothing, String] = readLine ?

  //
  // EXERCISE 3
  //
  // Using the `IO.syncException` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile1(file: File): IO[Exception, List[String]] =
    Source.fromFile(file).getLines.toList ?

  //
  // EXERCISE 3.5
  //
  // Using the `IO.syncThrowable` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile2(file: File): IO[Throwable, List[String]] =
    Source.fromFile(file).getLines.toList ?

  //
  // EXERCISE 3.75
  //
  // Using the `IO.syncCatch` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  import java.io.IOException
  def readFile3(file: File): IO[IOException, List[String]] =
    Source.fromFile(file).getLines.toList ?

  //
  // EXERCISE 4
  //
  // Identify the correct method and error type to import `System.nanoTime`
  // safely into the world of pure functional programming.
  //
  def nanoTime: IO[???, Long] = System.nanoTime() ?

  //
  // EXERCISE 5
  //
  // Identify the correct method, error, and value type to import `System.exit`
  // safely into the world of pure functional programming.
  //
  def sysExit(code: Int): IO[???, ???] = System.exit(code) ?

  //
  // EXERCISE 6
  //
  // Identify the correct method, error, and value type to import
  // `Array.update` safely into the world of pure functional programming.
  //
  def arrayUpdate[A](a: Array[A], i: Int, f: A => A): IO[???, ???] =
    a.update(i, f(a(i))) ?

  //
  // EXERCISE 7
  //
  // Use the `IO.async` method to implement the following `sleep` method, and
  // choose the correct error type.
  //
  val scheduledExecutor = Executors.newScheduledThreadPool(1)
  def sleep(l: Long, u: TimeUnit): IO[???, Unit] =
    scheduledExecutor.schedule(new Runnable {
      def run(): Unit = ???
    }, l, u) ?

  //
  // EXERCISE 8
  //
  // Translate the following procedural program into ZIO.
  //
  def playGame1(): Unit = {
    val number = scala.util.Random.nextInt(5)
    println("Enter a number between 0 - 5: ")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case None =>
        println("You didn't enter an integer!")
        playGame1
      case Some(guess) if (guess == number) =>
        println("You guessed right! The number was " + number)
      case _ =>
        println("You guessed wrong! The number was " + number)
    }
  }
  def playGame2: IO[Exception, Unit] = ???
}

object zio_concurrency {
  type ??? = Nothing

  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Race `leftContestent1` and `rightContestent1` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent1 = IO.never
  val rightContestent1 = putStrLn("Hello World")
  val raced1 = ???

  //
  // EXERCISE 2
  //
  // Race `leftContestent2` and `rightContestent2` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent2: IO[Exception, Nothing] = IO.fail(new Exception("Uh oh!"))
  val rightContestent2: IO[Exception, Unit] = IO.sleep(10.milliseconds) *> putStrLn("Hello World")
  val raced2: ??? = ???

  //
  // EXERCISE 3
  //
  // Compute `leftWork1` and `rightWork1` in parallel using the `par` method of
  // `IO`.
  //
  val leftWork1: IO[Nothing, Int] = fibonacci(10)
  val rightWork1: IO[Nothing, Int] = fibonacci(10)
  val par1: ??? = ???

  //
  // EXERCISE 4
  //
  // Compute all values `workers` in parallel using `IO.parAll`.
  //
  val workers: List[IO[Nothing, Int]] = (1 to 10).toList.map(fibonacci(_))
  val workersInParallel: IO[Nothing, List[Int]] =
    ???

  //
  // EXERCISE 5
  //
  // Implement `myPar` by forking `left` and `right`, and then joining them
  // and yielding a tuple of their results.
  //
  def myPar[E, A, B](left: IO[E, A], right: IO[E, B]): IO[E, (A, B)] =
    ???

  //
  // EXERCISE 6
  //
  // Use the `IO.supervise` method to ensure that when the main fiber exits,
  // all fibers forked within it will be terminated cleanly.
  //
  val supervisedExample: IO[Nothing, Unit] =
    (for {
      fiber <- fibonacci(10000).fork
    } yield ())

  //
  // EXERCISE 7
  //
  // Use the `interrupt` method of the `Fiber` object to cancel the long-running
  // `fiber`.
  //
  val interrupted1: IO[Nothing, Unit] =
    for {
      fiber <- fibonacci(10000).fork
    } yield ()

  //
  // EXERCISE 8
  //
  // Use the `seqWith` method of the `Fiber` object to combine `fiber1` and
  // `fiber2` into a single fiber (by summing the results), so they can be
  // interrupted together.
  //
  val interrupted2: IO[Nothing, Unit] =
    for {
      fiber1 <- fibonacci(10).fork
      fiber2 <- fibonacci(20).fork
      both = fiber1.zipWith(fiber2)(_ + _)
      _      <- both.interrupt
    } yield ()

  //
  // EXERCISE 9
  //
  // Use the `timeout` method of `IO` to time out the following long-lived
  // computation after 60 seconds.
  //
  val timedout: IO[Nothing, Option[Int]] = fibonacci(100) ?

  //
  // EXERCISE 10
  //
  // Use `IO.parTraverse` to compute the fibonacci numbers of the list of
  // integers in parallel.
  //
  val fibsToCompute = List(1, 2, 3, 4, 5, 6, 7)
  val inParallel: IO[Nothing, List[Int]] = IO.parTraverse(fibsToCompute)(fibonacci(_))

  def fibonacci(n: Int): IO[Nothing, Int] =
    if (n <= 1) IO.now(n)
    else fibonacci(n - 1).seqWith(fibonacci(n - 2))(_ + _)
}

object zio_resources {
  import java.io.{File, FileInputStream}
  class InputStream private (is: FileInputStream) {
    def read: IO[Exception, Option[Byte]] =
      IO.syncException(is.read).map(i => if (i < 0) None else Some(i.toByte))
    def close: IO[Exception, Unit] =
      IO.syncException(is.close())
  }
  object InputStream {
    def openFile(file: File): IO[Exception, InputStream] =
      IO.syncException(new InputStream(new FileInputStream(file)))
  }

  {
    println("Started")
    try {
      try throw new Error("Primary error")
      finally throw new Error("Secondary error")
    } catch {
      case e : Error => println(e)
    }
    println("Ended")
  }

  /*val acquire: IO[Exception, FileHandle]
  val release: FileHandle => IO[Nothing, Unit]
  val use    : FileHandle => IO[Exception, Result]

  acquire.bracket(release)(use)
  acquire.bracket(release) { fileHandle =>
    for {
      bytes1 <- readChunk(fileHandle)
      bytes2 <- readChunk(fileHandle)
    } yield bytes1 ++ bytes2
  }*/

  //
  // EXERCISE 1
  //
  // Rewrite the following procedural program to ZIO, using `IO.fail` and the
  // `bracket` method of the `IO` object.
  //
  def tryCatch1(): Unit =
    try throw new Exception("Uh oh")
    finally println("On the way out...")
  val tryCatch2: IO[Exception, Unit] =
    IO.fail(new Exception("Uh oh"))
      .bracket(_ => IO.sync(println("On the way out...")))(IO.now)

  //
  // EXERCISE 2
  //
  // Rewrite the `readFile1` function to use `bracket` so resources can be
  // safely cleaned up in the event of errors, defects, or interruption.
  //
  def readFile1(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    for {
      stream <- InputStream.openFile(file)
      bytes  <- readAll(stream, Nil)
    } yield bytes
  }
  def readFile2(file: File): IO[Exception, List[Byte]] ={

    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }
    InputStream.openFile(file).bracket(_.close.attempt.void)(readAll(_, Nil))
  }

  //
  // EXERCISE 3
  //
  // Implement the `tryCatchFinally` method using `bracket`.
  //
  def tryCatchFinally[E, A]
    (try0: IO[E, A])
    (catch0: PartialFunction[E, IO[E, A]])
    (finally0: IO[Nothing, Unit]): IO[E, A] =
    try0.catchSome(catch0).ensuring(finally0)

  //
  // EXERCISE 4
  //
  // Use the `bracket` method to rewrite the following snippet to ZIO.
  //
  def readFileTCF1(file: File): List[Byte] = {
    var fis : FileInputStream = null

    try {
      fis = new FileInputStream(file)
      val array = Array.ofDim[Byte](file.length.toInt)
      fis.read(array)
      array.toList
    } catch {
      case e : java.io.IOException => Nil
    } finally if (fis != null) fis.close()
  }
  def readFileTCF2(file: File): IO[Exception, List[Byte]] =
    ???
}

object zio_schedule {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  // Schedule[A,B]
  // consumes A, produces B and decide when to continue

  /*object Request {
    def apply() = new Request()
  }
  trait Response
  def makeRequest(input: Request): IO[Exception, Response] = ???

  val io2 = makeRequest(Request())

  val schedule = Schedule[Request, Response]

  io2 orElse io2 orElse io2 // can add delays or retries
  io2.retry(schedule)
  io2.repeatOrElse(schedule, _)
  */

  //
  // EXERCISE 1
  //
  // Using `Schedule.recurs`, create a schedule that recurs 5 times.
  //
  val fiveTimes: Schedule[Any, Int] =
  Schedule.recurs(5)

  //
  // EXERCISE 2
  //
  // Using the `repeat` method of the `IO` object, repeat printing "Hello World"
  // five times to the console.
  //
  val repeated1 = putStrLn("Hello World").repeat(fiveTimes)

  //
  // EXERCISE 3
  //
  // Using `Schedule.spaced`, create a schedule that recurs forever every 1
  // second.
  //
  val everySecond: Schedule[Any, Int] =
  Schedule.spaced(1.second)

  //
  // EXERCISE 4
  //
  // Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats fives times,
  // every second.
  //
  val fiveTimesEverySecond = fiveTimes && everySecond
  // && will repeat as long as both schedules agree, at every step will choose the max space/delay


  //
  // EXERCISE 5
  //
  // Using the `repeat` method of the `IO` object, repeat the action
  // putStrLn("Hi hi") using `fiveTimesEverySecond`.
  //
  val repeated2 = putStrLn("Hi hi").repeat(fiveTimesEverySecond)

  //
  // EXERCISE 6
  //
  // Using the `andThen` method of the `Schedule` object, the `fiveTimes`
  // schedule, and the `everySecond` schedule, create a schedule that repeats
  // fives times rapidly, and then repeats every second forever.
  //
  val fiveTimesThenEverySecond = fiveTimes andThen everySecond

  //
  // EXERCISE 7
  //
  // Using the `retry` method of the `IO` object, retry the following error
  // a total of five times.
  //

  // IO.repeat feeds values from successful IO
  // IO.retry feeds errors of failed IO

  val fiveTimesWaitOneSecLoop =
    Schedule.recurs(5) andThen (Schedule.spaced(1.second) && Schedule.recurs(1)).loop

  val error1 = IO.fail("Uh oh!")
  val retried5 = error1.retry(fiveTimes)

  //
  // EXERCISE 8
  //
  // Using the `||` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats the minimum
  // of five times and every second.
  //
  val fiveTimesOrEverySecond = fiveTimes || everySecond
  // continues as long as either one agrees and choose the min space

  //
  // EXERCISE 9
  //
  // Produce a jittered schedule that first does exponential spacing (starting
  // from 10 milliseconds), but then after the spacing reaches 60 seconds,
  // switches over to fixed spacing of 60 seconds between recurrences, but will
  // only do that for up to 100 times, and produce a list of the results.
  //
  val jitteredEverySecond = Schedule.spaced(1.second).jittered
  def mySchedule[A]: Schedule[A, List[A]] =
    (
      (Schedule.exponential(10.millisecond).whileValue(_ < 60.seconds)) andThen
      (Schedule.fixed(60.seconds) && Schedule.recurs(100))
    ).jittered *> Schedule.identity[A].collect
}

object zio_interop {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  import scala.concurrent.Future
  import scalaz.zio.interop.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  //
  // EXERCISE 1
  //
  // Use `IO.fromFuture` method to convert the following `Future` into an `IO`.
  //
  val future1 = () => Future.successful("Hello World")
  val io1 = IO.fromFuture(???)(global)

  //
  // EXERCISE 2
  //
  // Use the `toFuture` method on `IO` to convert the following `io` to `Future`.
  //
  val io2: IO[Throwable, Int] = IO.point(42)
  val future2: IO[Nothing, Future[Int]] = io2 ?

  //
  // EXERCISE 3
  //
  // Use the Fiber.fromFuture` method to convert the following `Future` into
  // an `IO`.
  //
  val future3 = () => Future.failed[Int](new Error("Uh ohs!"))
  val fiber1: Fiber[Throwable, Int] = Fiber.fromFuture(???)(global)

  import scalaz.zio.interop.Task
  import scalaz.zio.interop.catz._
  import cats.effect.concurrent.Ref

  //
  // EXERCISE 4
  //
  // The following example uses the `Ref` from `cats-effect`, demonstrating how
  // `cats-effect` structures work with ZIO.
  //
  class Worker(number: Int, ref: Ref[Task, Int]) {
    def work: Task[Unit] =
      for {
        c1 <- ref.get
        _  <- putStrLn(s"#$number >> $c1")
        c2 <- ref.modify(x => (x + 1, x))
        _  <- putStrLn(s"#$number >> $c2")
      } yield ()
  }

  val program: Task[Unit] =
    for {
      ref <- Ref.of[Task, Int](0)
      w1  = new Worker(1, ref)
      w2  = new Worker(2, ref)
      w3  = new Worker(3, ref)
      f   <- IO.forkAll(List(w1.work, w2.work, w3.work))
      _   <- f.join
    } yield ()
}

object zio_ref {
  // Ref is a volatile, atomic, purely functional `var`
  val zero: IO[Nothing, Ref[Int]] = Ref(0)

  // change value to be 10 times greater than initial value
  /*val t = for {
    ref <- zero
    value <- ref.get
    newValue <- value + 10
    _   <- ref.set(newValue)
  } yield newValue*/

  // use update to atomically increment by 10
  val atomicallyIncrementedBy10: IO[Nothing, Int] = for {
    ref <- zero
    newVal <- ref.update(current => current + 10)
  } yield newVal

  // increment atomically by 10 but return old variable
  val atomicallyIncrementedBy10PlusGet: IO[Nothing, Int] =
    for {
      ref       <- zero
      newValue  <- ref.modify(existing => (existing /*old*/, existing + 10 /*new*/)): IO[Nothing, Int]
    } yield newValue
}

object zio_promise extends RTS {
  // use make construct a promise that can't fail but can be completed with an integer
  val intPromise: IO[Nothing, Promise[Nothing, Int]] = Promise.make

  // use complete
  val completed1: IO[Nothing, Boolean] = for {
    promise <- intPromise
    completed <- promise.complete(5)
  } yield completed

  // use error
  /*val errored1: IO[Nothing, Boolean] = for {
    promise <- intPromise
    completed <- promise.error(/* can't return Nothing*/)
  } yield completed
  */

  val errored1: IO[Nothing, Boolean] = for {
    promise <- Promise.make[Error, String]
    completed <- promise.error(new Error("Boom!"))
  } yield completed

  val interrupted: IO[Nothing, Boolean] = for {
    promise <- Promise.make[Error, String]
    completed <- promise.interrupt
  } yield completed

  val handoff1: IO[Nothing, Int] =
    for {
      promise <- Promise.make[Nothing, Int]
      _       <- promise.complete(42).delay(10.milliseconds).fork
      value   <- promise.get
    } yield value

  val handoff2: IO[Error, Int] =
    for {
      promise <- Promise.make[Error, Int]
      _       <- (promise.error(new Error("Uh oh")).delay(10.milliseconds)).fork
      value   <- promise.get
    } yield value

  val handoff3: IO[Error, Int] =
    for {
      promise <- Promise.make[Error, Int]
      _       <- promise.interrupt.delay(10.milliseconds).fork
      value   <- promise.get
    } yield value

  def main(args: Array[String]): Unit = {
    println(unsafeRun(handoff1))
  }
}

object zio_queue {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `Queue.bounded`, create a queue for `Int` values with a capacity
  // of 10.
  //
  val makeQueue: IO[Nothing, Queue[Int]] =
    Queue.bounded(10)

  //
  // EXERCISE 2
  //
  // Using the `offer` method of `Queue`, place an integer value into a queue.
  //
  val offered1: IO[Nothing, Unit] =
  for {
    queue <- makeQueue
    _     <- queue.offer(42)
  } yield ()

  //
  // EXERCISE 3
  //
  // Using the `take` method of `Queue`, take an integer value from a queue.
  //
  val taken1: IO[Nothing, Int] =
    for {
      queue <- makeQueue
      _     <- queue.offer(42)
      value <- queue.take
    } yield value

  //
  // EXERCISE 4
  //
  // In one fiber, place 2 values into a queue, and in the main fiber, read
  // 2 values from the queue.
  //
  val offeredTaken1: IO[Nothing, (Int, Int)] =
    for {
      queue <- makeQueue
      _     <- (queue.offer(42) *> queue.offer(42)).fork
      v1    <- queue.take
      v2    <- queue.take
    } yield (v1, v2)

  //
  // EXERCISE 5
  //
  // In one fiber, read infintely many values out of the queue and write them
  // to the console. In the main fiber, write 100 values into a queue.
  //
  /*
  val infiniteReader1: IO[Nothing, List[Int]] =
    for {
      queue <- makeQueue
      _     <- (??? : IO[Nothing, Nothing]).fork
      vs    <- (queue ? : IO[Nothing, List[Int]])
    } yield vs
   */
  val infiniteReader1: IO[Nothing, List[Int]] = {
    def repeatedlyOffer[A](element: A, times: Int, queue: Queue[A]): IO[Nothing, List[A]] =
      if (times == 0) IO.point(Nil)
      else for {
        _       <- queue.offer(element)
        listInt <- repeatedlyOffer(element, times - 1, queue)
      } yield listInt :+ element

    for {
      queue <- makeQueue
      _     <- (queue.take.forever: IO[Nothing, Nothing]).fork
      vs    <- repeatedlyOffer(element = 42, times = 100, queue = queue): IO[Nothing, List[Int]]
    } yield vs
  }

  /*val infiniteReader2: IO[Nothing, List[Int]] =
    for {
      queue <- makeQueue
      _     <- (queue.take.flatMap(i => putStrLn(i.toString).attempt.void) forever: IO[Nothing, Nothing]).fork
      vs    <- (IO.sequence((1 to 100).toList.map(i => {
        queue.offer(i)
        i
      })))
    } yield vs*/

  //
  // EXERCISE 6
  //
  // Using `Queue`, `Ref`, and `Promise`, implement an "actor" like construct
  // that can atomically update the values of a counter.
  //
  /*
  val makeCounter: IO[Nothing, Int => IO[Nothing, Int]] =
    for {
      counter <- Ref(0)
      queue   <- Queue.bounded[(Int, Promise[Nothing, Int])](100)
      _       <- (queue.take ? : IO[Nothing, Fiber[Nothing, Nothing]])
    } yield { (amount: Int) =>
      ???
    }
  */

  val makeCounter: IO[Nothing, Int => IO[Nothing, Int]] =
    for {
      counter <- Ref(0)
      queue   <- Queue.bounded[(Int, Promise[Nothing, Int])](100)
      // consume from the queue and update the counter and then complete the promise to indicate
      // we have finished with the element we have pulled from the queue
      _       <- queue.take
        .flatMap{
          case (completeWith: Int, promise: Promise[Nothing, Int]) =>
            counter.update(existing => existing + completeWith) *> promise.complete(completeWith)
        }
        .forever
        .fork: IO[Nothing, Fiber[Nothing, Nothing]]
    } yield {
      // we yield a function for a way to send messages to the actor, let it do the work and get the updated value
      // only the actor deals with the Ref and communicates back with us (this piece of code below) the updated Ref value
      // using the Promise
      amount: Int =>
        for {
          promise   <- Promise.make[Nothing, Int]
          _         <- queue.offer((amount, promise))
          newValue  <- promise.get      // get the new value of the counter (this work is done by the "actor" fiber)
        } yield newValue
    }

  // create a producer
  val counterExample: IO[Nothing, Int] =
    for {
      counterFn <- makeCounter
      _         <- IO.parAll(List.fill(100)(IO.traverse(0 to 100)(counterFn)))
      value     <- counterFn(0) // call the counterFn with 0, this will happen 100 times in parallel thanks to parAll
    } yield value
}

object zio_rts {

  val MyRTS: RTS = new RTS{}

  val u1 : Unit = MyRTS.unsafeRun(putStrLn("Hello World!"))

  (MyRTS.unsafeRunSync(putStrLn("Hello World!"))): ExitResult[IOException, Unit]

  object MyApp extends App {
    def run(args: List[String]): IO[Nothing, ExitStatus] =
      (for {
        _ <- putStrLn("Hello World!")
        n <- getStrLn
        _ <- putStrLn(s"Hello $n")
      } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
  }
}

object zio_advanced {

}
