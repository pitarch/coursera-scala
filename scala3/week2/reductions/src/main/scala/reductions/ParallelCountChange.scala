package reductions

import org.scalameter._

import scala.annotation.tailrec

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    seqResult = ParallelCountChange.countChange(amount, coins)
    println(s"sequential result = $seqResult")
    val threshold = ParallelCountChange.totalCoinsThreshold(coins.length)
    parResult = ParallelCountChange.parCountChange(amount, coins, threshold)

    println(s"parallel result = $parResult")

    //    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try {
    //      val fjtime = standardConfig measure {
    //        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
    //      }
    //      println(s"parallel result = $parResult")
    //      println(s"parallel count time: $fjtime")
    //      println(s"speedup: ${seqtime.value / fjtime.value}")
    //    } catch {
    //      case e: NotImplementedError =>
    //        println("Not implemented.")
    //    }
    //
    //    println("\n# Using moneyThreshold\n")
    //    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    //    println("\n# Using totalCoinsThreshold\n")
    //    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    //    println("\n# Using combinedThreshold\n")
    //    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }


  //  def main(args: Array[String]): Unit = {
  //    val amount = 250
  //    val coins = List(1, 2, 5, 10, 20, 50)
  //    val seqtime = standardConfig measure {
  //      seqResult = ParallelCountChange.countChange(amount, coins)
  //    }
  //    println(s"sequential result = $seqResult")
  //    println(s"sequential count time: $seqtime")
  //
  //    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try {
  //      val fjtime = standardConfig measure {
  //        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
  //      }
  //      println(s"parallel result = $parResult")
  //      println(s"parallel count time: $fjtime")
  //      println(s"speedup: ${seqtime.value / fjtime.value}")
  //    } catch {
  //      case e: NotImplementedError =>
  //        println("Not implemented.")
  //    }
  //
  //    println("\n# Using moneyThreshold\n")
  //    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
  //    println("\n# Using totalCoinsThreshold\n")
  //    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
  //    println("\n# Using combinedThreshold\n")
  //    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  //  }
}

object ParallelCountChange extends ParallelCountChangeInterface {

  type CoinType = Int
  type Amount = Int

  /** Returns the number of ways change can be made from the specified list of
   * coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    @tailrec
    def internalCountChange(pending: Seq[(Amount, List[CoinType])], acc: Int = 0): Int = {

      val (nextAcc, nextPending) = pending match {
        case Nil => (acc, Nil)
        case (money, _) :: tail if money == 0 => (acc + 1, tail)
        //case (money, coin :: Nil) :: tail => if (money % coin == 0) (acc + 1, tail) else (acc, tail)
        case (_, Nil) :: tail => (acc, tail)
        case (money, coin :: coins) :: tail =>
          if (money >= coin) (acc, (money - coin, coin :: coins) :: (money, coins) :: tail)
          else (acc, (money, coins) :: tail)
      }
      if (nextPending == Nil) acc
      else internalCountChange(nextPending, nextAcc)
    }

    if (money == 0) 1
    else internalCountChange(Seq((money, coins)))
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   * specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {

    if (threshold(money, coins)) countChange(money, coins)
    else coins match {
      case coin :: coinTail if money >= coin =>
//        println(s"parCountChange(money:$money, coins:$coins)")
        val resultTuple = parallel(parCountChange(money - coin, coins, threshold), parCountChange(money, coinTail, threshold))
        resultTuple._1 + resultTuple._2
      case coin :: coinTail if money < coin =>
        parCountChange(money, coinTail, threshold)
      case _ :: Nil => countChange(money, coins)
      case Nil => 0
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (money: Int, _: List[Int]) => money <= startingMoney * 2 / 3


  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (_: Int, coins: List[Int]) => coins.length <= totalCoins * 2 / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    /*
    implement the method combinedThreshold, which returns a threshold function that returns true when the amount of
    money multiplied with the number of remaining coins is less than or equal to the starting money multiplied with
    the initial number of coins divided by 2:
     */
    (money: Int, coins: List[Int]) => //money * coins.length <= startingMoney * allCoins.length / 2
      money * coins.length <= startingMoney * allCoins.length / 2
  }
}
