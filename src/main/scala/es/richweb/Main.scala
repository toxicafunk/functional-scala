package es.richweb

import java.io.IOException

import scalaz.zio._
import scala.concurrent.duration._

object Main extends App {

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
      l <- yeller(args(0))
      l1 <- timesTen(l)
      _ <- program(l1)
    } yield ()
    )
    .repeat(Schedule.recurs(args(1).toInt) && Schedule.spaced(args(2).toInt.second))
    .redeemPure(_ => ExitStatus.ExitNow(1), s => ExitStatus.ExitNow(0))

}
