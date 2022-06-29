import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.std._

import scala.concurrent.duration.DurationInt
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File
import java.awt.color.ColorSpace
import java.awt.image.ColorConvertOp

object QueueExercise extends IOApp {
  case class ImageInfo(filepath: String, image: BufferedImage)

  def processImage(imageInfo: ImageInfo): ImageInfo = {
    val colorOp = new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_GRAY), null)
    val processedImage = colorOp.filter(imageInfo.image, imageInfo.image)
    imageInfo.copy(image = processedImage)
  }

  def saveImage(image: ImageInfo): IO[Unit] = {
    IO.println(s"Saving image ${image.filepath}...") *> IO.blocking {
      val fp = image.filepath
      val newPath = s"${fp.substring(0, fp.length - 4)}_processed.jpg"
      ImageIO.write(image.image, "jpg", new File(s"$newPath"))
    }.void
  }

  // please make sure directory exists
  def loadImages(directory: String): IO[List[ImageInfo]] = {
    for {
      dir    <- IO.blocking(new File(directory))
      files  <- IO.blocking(dir.listFiles.toList.filter(f => f.isFile && f.getName.endsWith(".jpg")))
      _ <- IO.println(s"Loading images from ${directory}...")
      images <- files.parTraverse(f => IO.blocking(ImageInfo(f.getAbsolutePath, ImageIO.read(f))))
    } yield images
  }

  // TODO: Take a processed image from the and save it to the corresponding file
  def imageSaver(
    processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] =
    IO.println("creating image saver... ") *>
      processedImageQueue.take.flatMap(saveImage)

  // TODO: Take a raw image from the queue, process it and put it in the processed queue
  def imageProcessor(
    rawImageQueue: Queue[IO, ImageInfo],
    processedImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] =
    IO.println("Creating imageProcessor...") *>
    rawImageQueue
      .take
      .map(processImage)
      .flatTap(img => IO.println(s"Processing image ${img.filepath}..."))
      .flatMap(processedImageQueue.offer)
      .foreverM

  // TODO: Load images from the dir and put them in the queue
  def imageLoader(
    srcDirectory: String,
    rawImageQueue: Queue[IO, ImageInfo]
  ): IO[Unit] = IO.println(s"Creating imageLoader for directory ${srcDirectory}") *>
    loadImages(srcDirectory)
      .flatMap(images =>
        IO.println(s"Loaded ${images.length} from directory $srcDirectory")
        *> images.parTraverse_(rawImageQueue.offer)
      )

  // TODO: Create the loaders, savers and processors and get them all running!
  def start(
    sourceDirs: List[String],
    noProcessors: Int,
    noSavers: Int
  ): IO[Unit] = {
    //    val rawImageQueue = Queue.unbounded[IO, ImageInfo]
    //    val processedImageQueue = Queue.unbounded[IO, ImageInfo]
    //    (rawImageQueue, processedImageQueue).parMapN{(rawQueue, processedQueue) =>
    Queue.unbounded[IO, ImageInfo].flatMap { rawQueue =>
      Queue.unbounded[IO, ImageInfo].flatMap { processedQueue =>
        val processing = (1 to noProcessors).toList.map(_ => imageProcessor(rawQueue, processedQueue))
        val saving = (1 to noSavers).toList.map(_ => imageSaver(processedQueue))
        val loading = sourceDirs.map(dir => imageLoader(dir, rawQueue))

        //              val processing = IO.defer(
        //                IO.println("a") *> (1 to noProcessors).toList.parTraverse_(_ => imageProcessor(rawQueue, processedQueue))
        //              )
        //              val saving = IO.defer(
        //                IO.println("b") *> (1 to noSavers).toList.parTraverse_(_ => imageSaver(processedQueue))
        //              )
        //              val loading = IO.defer(
        //                IO.println("c") *> sourceDirs.parTraverse_(dir => imageLoader(dir, rawQueue))
        //              )
        //              loading.both(processing).both(saving)
        //              processing.both(saving).both(loading).void
        //              (processing, saving, loading).parMapN((_, _, _) => ())
        (processing ++ saving ++ loading).parSequence_
      }
    }
  }
  override def run(args: List[String]): IO[ExitCode] = {
    val dirs = List("kittens", "puppies")
    start(dirs, 16, 16).timeoutTo(30.seconds, IO.unit).as(ExitCode.Success)
  }
}