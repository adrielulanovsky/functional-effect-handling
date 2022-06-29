import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.time.{Instant, LocalDateTime, ZoneId}
import scala.concurrent.duration.{DAYS, Duration, FiniteDuration}

object TimeExercise extends IOApp {
  def tomorrow(): IO[FiniteDuration] = IO.realTime.map(x => x.plus(Duration.create(1, DAYS)))
  def tomorrowDateTime(): IO[LocalDateTime] = tomorrow().map(x => LocalDateTime.ofInstant(Instant.ofEpochMilli(x.toMillis), ZoneId.systemDefault()))
  
  override def run(args: List[String]): IO[ExitCode] = {
    tomorrowDateTime().map(_.toString).flatTap(IO.println).as(ExitCode.Success)
  }
}