// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.applications

import java.util.concurrent.ExecutorService

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._
import scalaz._
import Scalaz._

import scala.language.higherKinds

object exercises extends App {
  case class CrawlState[E, A](visited: Set[URL], crawl: Crawl[E, A])
  object CrawlState {
    def visited[E: Monoid, A: Monoid](visited: Set[URL]): CrawlState[E, A] = CrawlState(visited, mzero[Crawl[E, A]])
    def crawled[E, A](crawl: Crawl[E, A]): CrawlState[E, A] = CrawlState(mzero[Set[URL]], crawl)

    implicit def MonoidCrawlState[E: Monoid, A: Monoid]: Monoid[CrawlState[E, A]] =
      new Monoid[CrawlState[E, A]] {
        def zero = CrawlState(mzero[Set[URL]], mzero[Crawl[E, A]])
        def append(l: CrawlState[E, A], r: => CrawlState[E, A]) = CrawlState(l.visited |+| r.visited, l.crawl |+| r.crawl)
      }
  }
  //
  // EXERCISE 1
  //
  // Implement the `crawlIO` function.
  //
  def crawlIO[E: Monoid, A: Monoid](
                                     seeds: Set[URL],
                                     router: URL => Set[URL],
                                     processor: (URL, String) => IO[E, A]): IO[Exception, Crawl[E, A]] = {

    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
    // update the Ref's CrawlState(visited) with the seeds that we are about to visit
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        // go through each of the seeds
        IO.traverse(seeds) { seed =>
          // visit the URL to get back HTML
          getURL(seed).redeem(
            _ => IO.unit,   // ignore errors
            html =>         // we get back some HTML
              for {
                acc     <- ref.get
                // get all the URLs from the HTML content (href tags)
                // use the router to determine what/what not to visit and remove all URLs we have already visited
                seeds   <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- acc.visited)
                // obtain crawl information from the HTML and Seed combo
                crawl   <- processor(seed, html).redeemPure(e => Crawl(e, mzero[A]),
                  a => Crawl(mzero[E], a))
                // update the Ref's existing crawl with the new crawled information
                _ <- ref.update(_ |+| CrawlState.crawled(crawl))
                // repeat with the new list of seeds,
                // Remember that traverse will do none of these computations if there are no elements to traverse
                _ <- loop(seeds, ref)
              } yield ()
          )
        }.void  // discard the output in IO[E, A] making it IO[E, Unit]

    for {
      ref   <- Ref(mzero[CrawlState[E, A]]) // initialize an empty Crawl Ref
      _     <- loop(seeds, ref)             // begin crawling
      state <- ref.get                      // get the final crawl state after crawling
    } yield state.crawl
  }

  //
  // EXERCISE 2
  //
  // Implement a version of the `crawlIO` function that works in parallel.
  //
  def crawlIOPar[E: Monoid, A: Monoid](
                                        seeds     : Set[URL],
                                        router    : URL => Set[URL],
                                        processor : (URL, String) => IO[E, A]): IO[Exception, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
    // update the Ref's CrawlState(visited) with the seeds that we are about to visit
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        // go through each of the seeds in parallel
        IO.parTraverse(seeds) { seed =>
          // visit the URL to get back HTML
          getURL(seed).redeem(
            _ => IO.unit,   // ignore errors
            html =>         // we get back some HTML
              for {
                visited <- ref.get.map(_.visited)
                // get all the URLs from the HTML content (href tags)
                // use the router to determine what/what not to visit and remove all URLs we have already visited
                seeds   <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- visited)
                // obtain crawl information from the HTML and Seed combo
                crawl   <- processor(seed, html).redeemPure(e => Crawl(e, mzero[A]),
                  a => Crawl(mzero[E], a))
                // update the Ref's existing crawl with the new crawled information
                _       <- ref.update(_ |+| CrawlState.crawled(crawl))
                // repeat with the new list of seeds,
                // Remember that traverse will do none of these computations if there are no elements to traverse
                _       <- loop(seeds, ref)
              } yield ()
          )
        }.void  // discard the output in IO[E, A] making it IO[E, Unit]

    for {
      ref   <- Ref(mzero[CrawlState[E, A]]) // initialize an empty Crawl Ref
      _     <- loop(seeds, ref)             // begin crawling
      state <- ref.get                      // get the final crawl state after crawling
    } yield state.crawl
  }

  //
  // EXERCISE 3
  //
  // Implement a version of the `crawlIOPar` that can be tested without having
  // to interact with the real world.
  //
  def crawlIO2[E: Monoid, A: Monoid](
                                      seeds     : Set[URL],
                                      router    : URL => Set[URL],
                                      processor : (URL, String) => IO[E, A],
                                      getURL    : URL => IO[Exception, String] = getURL): IO[Exception, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
    // update the Ref's CrawlState(visited) with the seeds that we are about to visit
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        // go through each of the seeds in parallel
        IO.parTraverse(seeds) { seed =>
          // visit the URL to get back HTML
          getURL(seed).redeem(
            _ => IO.unit,   // ignore errors
            html =>         // we get back some HTML
              for {
                visited <- ref.get.map(_.visited)
                // get all the URLs from the HTML content (href tags)
                // use the router to determine what/what not to visit and remove all URLs we have already visited
                seeds   <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- visited)
                // obtain crawl information from the HTML and Seed combo
                crawl   <- processor(seed, html).redeemPure(e => Crawl(e, mzero[A]),
                  a => Crawl(mzero[E], a))
                // update the Ref's existing crawl with the new crawled information
                _ <- ref.update(_ |+| CrawlState.crawled(crawl))
                // repeat with the new list of seeds,
                // Remember that traverse will do none of these computations if there are no elements to traverse
                _ <- loop(seeds, ref)
              } yield ()
          )
        }.void  // discard the output in IO[E, A] making it IO[E, Unit]

    for {
      ref   <- Ref(mzero[CrawlState[E, A]]) // initialize an empty Crawl Ref
      _     <- loop(seeds, ref)             // begin crawling
      state <- ref.get                      // get the final crawl state after crawling
    } yield state.crawl
  }

  final case class Crawl[E, A](error: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)
    def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
  }
  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]] {
        def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
        def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.error |+| r.error, l.value |+| r.value)
      }
  }

  final case class URL private (parsed: io.lemonlabs.uri.Url) {
    import io.lemonlabs.uri._

    final def relative(page: String): Option[URL] =
      scala.util.Try(parsed.path match {
        case Path(parts) =>
          val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

          parsed.withPath(UrlPath(whole))
      }).toOption.map(new URL(_))

    def url: String = parsed.toString

    override def equals(obj: Any): Boolean = obj match {
      case that: URL => this.url == that.url
      case _ => false
    }

    override def hashCode(): Int = url.hashCode
  }

  object URL {
    def apply(url: String): Option[URL] =
      scala.util.Try(io.lemonlabs.uri.AbsoluteUrl.parse(url)).toOption match {
        case None => None
        case Some(parsed) => Some(new URL(parsed))
      }
  }

  private val blockingThreadPool: ExecutorService = java.util.concurrent.Executors.newCachedThreadPool()
  def getURL(url: URL): IO[Exception, String] =
    for {
      promise <-  Promise.make[Exception, String]   // main fiber
      _       <-  (for {
        exitResult <- IO.async[Nothing, ExitResult[Exception, String]](callback =>
          // run computation on blockingThreadPool
          blockingThreadPool.submit(new Runnable {
            override def run(): Unit = {
              try {
                callback(
                  ExitResult.Completed(
                    ExitResult.Completed(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)
                  )
                )
              } catch {
                case e: Exception => callback(ExitResult.Completed(ExitResult.Failed(e)))
              }
            }
          })): IO[Nothing, ExitResult[Exception, String]]
        _         <- promise.done(exitResult)
      } yield ()).fork  // don't block the main fiber, run this on a separate fiber
      html    <-  promise.get       // suspend until we get the promise back
    } yield html

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try({
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

      for {
        m   <- matches
        url <- URL(m).toList ++ root.relative(m).toList
      } yield url
    }).getOrElse(Nil)
  }

  object testing {
    // okay to have partial functions in tests because it will blow up and our CI solution will know
    val Home          = URL("http://scalaz.org").get
    val Index         = URL("http://scalaz.org/index.html").get
    val ScaladocIndex = URL("http://scalaz.org/scaladoc/index.html").get
    val About         = URL("http://scalaz.org/about").get

    val SiteIndex =
      Map(
        Home -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        Index -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
        About -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
      )

    val getURL: URL => IO[Exception, String] =
      (url: URL) => SiteIndex.get(url).fold[IO[Exception, String]](
        IO.fail(new Exception(s"Could not connect to $url"))
      )(IO.now)

    val ScalazOnlyRouter: URL => Set[URL] = url =>
      if (url.parsed.apexDomain == Some("scalaz.org")) Set(url)
      else Set()

    val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
      (url, html) => IO.now(List(url -> html))
  }

  // Exercise 4
  // Create a type class for printLine and readLine
  // F has kind: * -> *
  trait Console[F[_]] {
    def readLine: F[String]
    def printLine(msg: String): F[Unit]
  }

  object  Console {
    def apply[F[_]](implicit C: Console[F]): Console[F] = C
  }

  // Exercise 5
  // implement helper methods that work with any F[_] that support the Console effect
  def printLine[F[_]: Console](line: String): F[Unit] = Console[F].printLine(line)
  def readLine[F[_]: Console]: F[String] = Console[F].readLine

  // Exercise 6
  // Create an instance of the `Console` typeclass for `IO[E, ?]` for any `E` using IO.sync and Scala's println and
  // scala.io.StdIn.readLine
  implicit def ConsoleIO[E]: Console[IO[E, ?]] =
    new Console[IO[E, ?]] {
      override def readLine: IO[E, String] = IO.sync(scala.io.StdIn.readLine())

      override def printLine(msg: String): IO[E, Unit] = IO.sync(println(msg))
    }

  // Exercise 7
  // Create an instance of the Random type class for `IO[E, ?] for any `E` using IO.sync and scala.util.Random.nextInt
  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }

  object Random {
    def apply[F[_]](implicit R: Random[F]): Random[F] = R
  }

  def nextInt[F[_]: Random](max: Int): F[Int] = Random[F].nextInt(max)
  implicit def RandomIO[E]: Random[IO[E, ?]] = new Random[IO[E, ?]] {
    override def nextInt(max: Int): IO[E, Int] = IO.sync(scala.util.Random.nextInt(max))
  }

  // Exercise 8
  // Create a game that is polymorphic in the effect type F[_] requiring the capability to only perform
  // Console and Random effects
  def myGame[F[_]: Console: Random: Monad]: F[Unit] = {
    val Dictionary = List("scalaz", "monad", "typeclass", "tagless", "functor", "functional")
    case class State(username: String, guesses: Set[Char], word: String) {
      def failureCount: Int = (guesses -- word.toSet).size
      def playerLost: Boolean = guesses.size > word.length * 2
      def playerWon: Boolean = (word.toSet -- guesses).isEmpty
    }

    def choose[F[_]: Random: Functor]: F[String] =
      nextInt[F](Dictionary.length).map(Dictionary.apply(_))

    // Use Apply because we aren't really doing dependent operations
    def getName[F[_]: Console: Apply]: F[String] =
      printLine[F]("Please enter your name") *> readLine[F]

    def renderState[F[_]: Console](state: State): F[Unit] = {
      val word = state.word.toList.map { c =>
        if (state.guesses.contains(c)) s" $c "
        else "   "
      }.mkString("")
      val line = List.fill(state.word.length)(" - ").mkString("")
      val guesses = state.guesses.mkString(start = "Guesses: ", sep = ", ", end = "")
      val text = s"$word\n$line\n\n$guesses"
      printLine[F](text)
    }

    def getChoice[F[_]: Console: Monad]: F[Char] =
      for {
        _     <-  printLine[F]("Please enter a guess: ")
        guess <-  readLine[F].flatMap { line =>
          val trimmed = line.trim
          line.headOption match {
            case Some(choice) if choice.isLetter && line.length == 1 =>
              choice.point[F]
            case _ => printLine[F]("Your choice is not valid please try again") *> getChoice[F]
          }
        }
      } yield guess

    def gameLoop[F[_]: Console: Random: Monad](state: State): F[State] =
      for {
        choice  <- getChoice[F]
        state   <- state.copy(guesses = state.guesses + choice).point[F]
        state   <- renderState[F](state) *> (
          if (state.playerLost)
            printLine[F](s"You have been hanged, sorry ${state.username}, the word was ${state.word}") *> state.point[F]
          else if (state.playerWon)
            printLine[F](s"Congratulations, you are a winner, ${state.username} !") *> state.point[F]
          else if (state.word.toSet.contains(choice))
            printLine[F](s"You guessed correctly!, Keep at it ${state.username}") *> gameLoop[F](state)
          else
            printLine[F](s"You guessed incorrectly, keep trying!") *> gameLoop[F](state)
          )
      } yield state

    for {
      _     <- printLine[F]("Welcome to Purely Functional Hangman!")
      name  <- getName[F]
      word  <- choose[F]
      _     <- gameLoop[F](State(name, Set.empty[Char], word))
    } yield ()
  }

  // Exercise 9, implement the game for F=IO[Nothing, ?]
  val myGameIO: IO[Nothing, Unit] = myGame[IO[Nothing, ?]]

  // Exercise 10
  // create a test data structure that can contain a buffer of lines (to be read from the console), a log of output
  // and a list of random numbers
  case class TestData(output: List[String], input: List[String], random: List[Int]) {
    def render: String =
      s"OUTPUT:\n ${output.reverse.mkString("\n")}"
  }

  // Exercise 11
  // implement the following dynamically-created instance. Effects should be implemented in terms of modifying the passed
  // in Ref
  type GameEffects[F[_]] = Console[F] with Random[F] with Monad[F]
  case class TestIO[+E, A](run: IO[E, A])
  def createTestInstance(ref: Ref[TestData]): GameEffects[TestIO[Nothing, ?]] =
    new Console[TestIO[Nothing, ?]] with Random[TestIO[Nothing, ?]] with Monad[TestIO[Nothing, ?]] {
      override def bind[A, B](fa: TestIO[Nothing, A])(f: A => TestIO[Nothing, B]): TestIO[Nothing, B] =
        TestIO(fa.run.flatMap(f.andThen(_.run)))

      override def point[A](a: => A): TestIO[Nothing, A] = TestIO(IO.point(a))

      override def readLine: TestIO[Nothing, String] = TestIO(ref.modify(data => (data.input.head, data.copy(input = data.input.tail))))
      override def printLine(msg: String): TestIO[Nothing, Unit] = TestIO(ref.update(data => data.copy(output = data.output :+ msg)).void)
      override def nextInt(max: Int): TestIO[Nothing, Int] = TestIO(ref.modify(data => (data.random.head, data.copy(random = data.random.tail))))
    }

  // Exercise 12
  // Implement the following runner function which will run the game using the provided set of input test data
  // Note: if you do not provide enough of input to end the game, this will fail with an exception
  def testGame(testData: TestData): IO[Nothing, TestData] =
    for {
      ref  <- Ref(testData)
      _    <- {
        // because myGame uses context bound syntax, we do this
        implicit val instance = createTestInstance(ref)
        myGame[TestIO[Nothing, ?]]
      }.run
      data <- ref.get
    } yield data

  val GameTest1: IO[Nothing, String] = testGame(TestData(
    input = List("John", "s", "c", "a", "l", "a", "z"),
    output = List(),
    random = List(0)
  )).map(_.render)

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _     <- putStrLn("Hello World!")
      res   <- crawlIO2(Set(testing.Home), testing.ScalazOnlyRouter, testing.Processor, testing.getURL)
      _     <- putStrLn(s"Obtained ${res.value.mkString("\n")}")
      rend  <- GameTest1
      _     <- putStrLn(rend)
      //      _   <- myGameIO
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}

sealed trait Free[F[_], A] { self =>
  final def map[B](f: A => B): Free[F, B] = self.flatMap(f.andThen(Free.point[F, B](_)))

  final def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free.FlatMap(self, f)

  final def <* [B](that: Free[F, B]): Free[F, A] =
    self.flatMap(a => that.map(_ => a))

  final def *> [B](that: Free[F, B]): Free[F, B] =
    self.flatMap(_ => that)

  final def fold[G[_]: Monad](interpreter: F ~> G): G[A] =
    self match {
      case Free.Return(value0)  => value0().point[G]
      case Free.Effect(fa)      => interpreter(fa)
      case Free.FlatMap(fa0, f) => fa0.fold(interpreter).flatMap(a0 => f(a0).fold(interpreter))
    }
}
object Free {
  case class Return[F[_], A](value0: () => A) extends Free[F, A] {
    lazy val value = value0()
  }
  case class Effect[F[_], A](effect: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A0, A](fa0: Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

  def point[F[_], A](a: => A): Free[F, A] = Return(() => a)
  def lift[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)
}
object FreeTest {

  sealed trait ConsoleF[A]

  final case object ReadLine extends ConsoleF[String]

  final case class PrintLine(line: String) extends ConsoleF[Unit]

  def readLine: Free[ConsoleF, String] = Free.lift[ConsoleF, String](ReadLine)

  def printLine(line: String): Free[ConsoleF, Unit] = Free.lift[ConsoleF, Unit](PrintLine(line))

  val program: Free[ConsoleF, String] =
    for {
      _ <- printLine("Good morning! What is your name?")
      name <- readLine
      _ <- printLine("Good to meet you, " + name + "!")
    } yield name

  import scalaz.zio.IO
  import scalaz.zio.interop.scalaz72._

  val programIO: IO[Nothing, String] =
    program.fold[IO[Nothing, ?]](new NaturalTransformation[ConsoleF, IO[Nothing, ?]] {
      def apply[A](consoleF: ConsoleF[A]): IO[Nothing, A] =
        consoleF match {
          case ReadLine => IO.sync(scala.io.StdIn.readLine())
          case PrintLine(line) => IO.sync(println(line))
        }
    })

  case class TestData(input: List[String], output: List[String])

  case class State[S, A](run: S => (S, A)) {
    def eval(s: S): A = run(s)._2
  }

  object State {
    implicit def MonadState[S]: Monad[State[S, ?]] =
      new Monad[State[S, ?]] {
        def point[A](a: => A): State[S, A] = State(s => (s, a))

        def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
          State[S, B](s => fa.run(s) match {
            case (s, a) => f(a).run(s)
          })
      }

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

    def modify[S](f: S => S): State[S, Unit] =
      get[S].flatMap(s => set(f(s)))
  }

  val programState: State[TestData, String] =
    program.fold[State[TestData, ?]](new NaturalTransformation[ConsoleF, State[TestData, ?]] {
      def apply[A](consoleF: ConsoleF[A]): State[TestData, A] =
      /*_*/
        consoleF match {
          case ReadLine =>
            for {
              data <- State.get[TestData]
              line = data.input.head
              _ <- State.set(data.copy(input = data.input.drop(1)))
            } yield line

          case PrintLine(line) =>
            State.modify[TestData](d => d.copy(output = line :: d.output))
        }
      /*_*/
    })

  programState.eval(TestData("John" :: Nil, Nil))
}
