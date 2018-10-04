package es.richweb

import java.io.IOException

import scalaz.zio._
import scalaz.zio.console._
import scalaz._
import Scalaz._
import scalaz.zio.interop.scalaz72._

object Main extends App {

  implicit val ShowInt: Show[Int] = new Show[Int] {
    override def shows(f: Int): String = f.toString
  }

  implicit val ShowString: Show[String] = new Show[String] {
    override def shows(f: String): String = f
  }

  def write[A]: A => IO[Nothing, Unit] = (s: A) => IO.sync(println(s"${s} at ${System.currentTimeMillis()}"))

  val program: IO[IOException, String] = for {
    msg <- IO.point("Hello World!")
    _   <- write(msg)
  } yield msg

  def transformer[A](f: String => A): IO[IOException, String] => IO[IOException, A] = input => {
    input.map(f)
  }

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (transformer((_.size))(program)).flatMap(i => write(i.toString))
      .repeat(Schedule.recurs(5))
      .redeemPure(_ => ExitStatus.ExitNow(1), s => ExitStatus.ExitNow(0))
}
