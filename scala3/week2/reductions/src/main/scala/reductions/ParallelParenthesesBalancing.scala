package reductions

import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var opened: Int = 0
    chars foreach {
      case c if c == '(' => opened += 1
      case c if c == ')' && opened > 0 => opened -= 1
      case c if c == ')' => return false
      case _ =>
    }
    opened == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  //  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
  //
  //    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
  //      ???
  //    }
  //
  //    def reduce(from: Int, until: Int) /*: ???*/ = {
  //      ???
  //    }
  //
  //    reduce(0, chars.length) == ???
  //  }

  // For those who want more:
  // Prove that your reduction operator is associative!

  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    case class Unbalanced(opened: Int, closed: Int)

    def traverse(idx: Int, until: Int): Unbalanced = {
      var opened = 0
      var closed = 0
      var i = idx
      while (i < until) {
        chars(i) match {
          case '(' => opened = opened + 1
          case ')' => if (opened > 0) opened = opened - 1 else closed = closed + 1
          case _ =>
        }
        i = i + 1
      }
      Unbalanced(opened, closed)
    }

    def reduce(from: Int, until: Int): Unbalanced = {
      if (until - from > threshold) {
        val middle = from + (until - from) / 2
//        println(s"Parelilze: [$from, $until] => [$from, $middle] - [$middle, $until]")
        val (first, second) = parallel(reduce(from, middle), reduce(middle, until))
        val incr = if (first.opened >= second.closed) Unbalanced(first.opened - second.closed, 0) else Unbalanced(0, second.closed - first.opened)
        val result = Unbalanced(second.opened + incr.opened , first.closed + incr.closed)
//        println(s"[$from, $middle]/$first  *  [$middle, $until]/$second ~=> $result")
        result
      } else {
//        println(s"traverse: [$from, $until]")
        traverse(from, until)
      }
    }

    val result = reduce(0, chars.length)
//    println(s"result: $result")
    result == Unbalanced(0,0)
  }

}
