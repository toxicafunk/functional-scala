package es.richweb

import scalaz.zio._

object Main extends App {

  val write: String => IO[Nothing, Unit] = (s: String) => IO.sync(println)

  val program: IO[Nothing, String] = for {
    msg <- IO.now("Hello World!")
  } yield msg

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    program.redeemPure(_ => ExitStatus.ExitNow(1), s => { println(s); ExitStatus.ExitNow(0) })
}
