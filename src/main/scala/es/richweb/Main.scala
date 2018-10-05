package es.richweb

import java.io.IOException

import scalaz.zio._
import scala.concurrent.duration._

object Main extends App {

  val threeTimes: Schedule[Any, Int] =
    Schedule.recurs(3)

  val everyThreeSecond: Schedule[Any, Int] =
    Schedule.spaced(3.second)

  val fiveTimesThenEveryThreeSecond: Schedule[Any, Either[Int, Int]] = threeTimes andThen everyThreeSecond

  // Produce a jittered schedule that first does exponential spacing (starting
  // from 10 milliseconds), but then after the spacing reaches 60 seconds,
  // switches over to fixed spacing of 60 seconds between recurrences, but will
  // only do that for up to 100 times, and produce a list of the results.
  //
  def mySchedule[A]: Schedule[A, List[A]] =
    (
      Schedule.exponential(10.millisecond).whileValue(_ < 60.seconds) andThen
        (Schedule.fixed(60.seconds) && Schedule.recurs(3))
      ).jittered *> Schedule.identity[A].collect

  def write[A]: A => IO[Nothing, Unit] = (s: A) => IO.sync(println(s"$s (${System.currentTimeMillis()})"))

  def program[A]: A => IO[IOException, String] = input => for {
    msg <- IO.point(input)
    _ <- write(msg)
  } yield msg.toString

  def transformString[A](f: String => A): IO[IOException, String] => IO[IOException, A] = input => input.map(f)

  val lengther: String => IO[IOException, Int] = transformString(_.length) compose program

  val yeller: String => IO[IOException, String] = transformString(_.toUpperCase()) compose program

  val timesTen: String => IO[IOException, Int] = ((input: IO[IOException, Int]) => input.map(10 * _)) compose lengther

  def run(args: List[String]): IO[Nothing, ExitStatus] = (
    for {
      l <- yeller(args.head)
      l1 <- timesTen(l)
      l2 <- program(l1)
    } yield l2
    )
    .repeat(mySchedule)
    .flatMap(write)
    .redeemPure(_ => ExitStatus.ExitNow(1), s => ExitStatus.ExitNow(0))

}
