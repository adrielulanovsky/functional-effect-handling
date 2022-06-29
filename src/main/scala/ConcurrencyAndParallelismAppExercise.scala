import cats.effect._
import cats.implicits._

import scala.concurrent.duration.DurationInt

object ConcurrencyAndParallelismAppExercise extends IOApp {
  case class Quote(author: String, text: String)

  def fetchHttp(n: Int): IO[List[Quote]] =
    IO.sleep(10.seconds) *>
      (1 to n).toList.map(i => Quote(s"author $i", s"text $i")).pure[IO]

  def fetchDb(n: Int): IO[List[Quote]] =
    IO.sleep(100.millis) *>
      (1 to n).toList.map(i => Quote(s"author $i", s"text $i")).pure[IO]

  def fetchAuthorAge(author: String): IO[Int] =
    IO.sleep(150.millis) *> IO((math.random() * 100).toInt)


  override def run(args: List[String]): IO[ExitCode] = {
    val n = 3

    val x = IO.blocking()

    // fetch n quotes from the fastest source
    // calculate the average age of the authors
    IO.race(fetchHttp(n), fetchDb(n))
      .map(_.fold(identity, identity))
      .flatMap(_.parTraverse(q => fetchAuthorAge(q.author)))
      .flatTap(ages => IO.println(ages))
      .as(ExitCode.Success)

  }
}
