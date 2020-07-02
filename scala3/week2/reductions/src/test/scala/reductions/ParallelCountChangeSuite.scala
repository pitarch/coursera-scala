package reductions

import org.junit.Assert.assertEquals
import org.junit._

class ParallelCountChangeSuite {


  /*******************************
   * PARALLEL COUNT CHANGE SUITE *
   *******************************/

  import ParallelCountChange._

  @Test def `parCountChange should return 0 for money < 0`: Unit = {
    def check(money: Int, coins: List[Int]) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  @Test def `parCountChange should return 1 when money == 0`: Unit = {
    def check(coins: List[Int]) =
      assert(parCountChange(0, coins, combinedThreshold(0, coins)) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  @Test def `parCountChange should return 0 for money > 0 and coins = List()`: Unit = {
    def check(money: Int) =
      assert(parCountChange(money, List(), combinedThreshold(money, List())) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  @Test def `parCountChange should work when there is only one coin`: Unit = {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  @Test def `parCountChange should work for multi-coins`: Unit = {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  @Test def `parCountChange with combinedThreshold should produce correct result when there is only one coin and the amount is equal to or less than the value of the coin`: Unit = {

    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(2, List(2), 1)
    check(1, List(250), 0)
  }


  @Test def `parCountChange with totalCoinsThreshold should produce correct output when there are six coins and the amount is 250`() = {

    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, totalCoinsThreshold(250)) == expected,
        s"countChange($money, $coins) should be $expected")

    check(250, List(1,2,5,10,20,50), 177863)

  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

