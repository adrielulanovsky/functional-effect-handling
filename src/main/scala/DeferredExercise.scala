import cats.effect._
import cats.effect.implicits._
import cats.implicits._

object DeferredExercise extends IOApp {
  class Producer[A](name: String, deferred: Deferred[IO, A], exec: IO[A]) {
    def run(): IO[Unit] = IO.println(s"Producer ${name} running") *>
      exec.flatMap(a => deferred.complete(a)).void
  }

  class Consumer[A](name: String, deferred: Deferred[IO, A], consume: A => IO[Unit]) {
    def run(): IO[Unit] = IO.println(s"Consumer ${name} running") *>
      deferred.get.flatMap(consume)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    Deferred[IO, String].flatMap{ deferred =>
      val producer = new Producer[String]("hola", deferred, IO.println("1").as("asd"))
      val consumer = new Consumer[String]("chau", deferred, IO.println[String])
      consumer.run().both(producer.run()).void
    } *>
    IO.pure(ExitCode.Success)
  }
}