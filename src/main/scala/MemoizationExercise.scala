import cats.effect.IO.pure
import cats.effect._
import cats.implicits._

import scala.concurrent.duration.DurationInt

object MemoizationExercise extends IOApp {
  trait Currency
  case object Dollar extends Currency
  case object Euro extends Currency

  case class Balance(amount: Double, currency: Currency)

  // pretend this is calling an API and takes some time
  def fetchDollarExchangeRate(currency: Currency): IO[Double] =
    IO.println(s"Fetching exchange rates for ${currency}") *>
    IO.sleep(2000.millis) *>
    IO.pure {
      currency match {
        case Dollar => 1.0
        case Euro => 1.12
      }
    }

  val euroExchangeRate: IO[Double] = fetchDollarExchangeRate(Euro)

  def getBalancesInDollars(balances: List[Balance]): IO[List[Double]] = euroExchangeRate.memoize.flatMap { exchangeRateIO =>
    balances.parTraverse(balance => balance.currency match {
      case Dollar => balance.amount.pure[IO]
      case Euro => exchangeRateIO.map { exchangeRate =>
        balance.amount * exchangeRate
      }
    })
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val balances = List(Balance(2.0, Dollar), Balance(3.0, Dollar))
    getBalancesInDollars(balances).flatTap(balances => IO.println(balances)).as(ExitCode.Success)
    // Modify both functions so they return an IO
    // Achieve the same behaviour:
    // - If all balances are dollars, you never fetch the exchange rate
    // - If more than one balance is euros, you only fetch the exchange rate once
  }
}