import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

object TaglessFinalExercise extends IOApp {
  case class Image(bytes: List[Byte])

  trait ImagesService[F[_]] {
    def fetchHttp(n: Int): F[List[Image]]
    def fetchDb(n: Int): F[List[Image]]
    def fetchBoth(n: Int): F[List[Image]]
  }

  object ImagesService {
    def impl[F[_]: Sync: Parallel]: ImagesService[F] = new ImagesService[F] {
      override def fetchHttp(n: Int): F[List[Image]] = {
        List.range(0, n).parTraverse { i =>
          Sync[F].blocking(Image(List(i.toByte)))
        }
      }

      override def fetchDb(n: Int): F[List[Image]] = {
        List.range(0, n).parTraverse { i =>
          Sync[F].blocking(Image(List((100 + i).toByte)))
        }
      }

      override def fetchBoth(n: Int): F[List[Image]] =
        (fetchDb(n), fetchHttp(n)).parMapN { case (dbImages, httpImages) => dbImages ++ httpImages }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val imagesService = ImagesService.impl[IO]
    imagesService.fetchBoth(10).flatTap(IO.println).as(ExitCode.Success)
  }
}
