import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.util.concurrent.LinkedBlockingQueue

// Hints:
// - LinkedBlockingQueue for tasks
// - Workers run forever
object ThreadPoolExercise {
  class FixedThreadPool(noThreads: Int) {
    val tasks = new LinkedBlockingQueue[Runnable]()
    val workers = (1 to noThreads).toList.map(i => new Worker(s"fixed-thread-pool-${i.toString}"))

    workers.foreach(_.start())

    def execute(runnable: Runnable): Unit = {
      tasks.put(runnable)
    }

    class Worker(name: String) extends Thread(name) {
      override def run(): Unit = {
        while(true) {
          tasks.take().run()
        }
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val threadPool = new FixedThreadPool(3)
    (1 to 10).foreach(i =>
      threadPool.execute(() => println(s"Current thread: ${Thread.currentThread().getName}"))
    )

  }
}
