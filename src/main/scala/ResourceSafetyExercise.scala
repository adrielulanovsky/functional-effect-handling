import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.io._
import java.net.{HttpURLConnection, URL, URLConnection}

object ResourceSafetyExercise extends IOApp {
  def makeReader(inputStream: InputStream): Resource[IO, BufferedReader] = {
    Resource.fromAutoCloseable(IO.blocking(new BufferedReader(new InputStreamReader(inputStream))))
  }

  def makeConnection(targetURL: String): Resource[IO, HttpURLConnection] = {
    Resource.make(createConnection(targetURL))(conn => IO.blocking(conn.disconnect()))
  }

  def makeResources(targetURL: String): Resource[IO, (HttpURLConnection, BufferedReader)] = {
    for {
      connection <- makeConnection(targetURL)
      reader <- makeReader(connection.getInputStream)
    } yield (connection, reader)
  }

  def createConnection(targetURL: String): IO[HttpURLConnection] =
    IO.blocking {
      val connection = new URL(targetURL).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection
    }
  def readOutput(reader: BufferedReader): IO[String] =
    IO.blocking {
      Iterator
        .continually(reader.readLine)
        .takeWhile(_ != null)
        .mkString("\n")
    }

  def httpGet(targetURL: String): IO[String] = {
    makeResources(targetURL).use{ case (_, reader) =>
      readOutput(reader)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    httpGet("http://www.google.com")
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
