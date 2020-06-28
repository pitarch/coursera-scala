package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */

  def pascal(c: Int, r: Int): Int = {

    @tailrec
    def visit(acc: Int, pending: List[(Int, Int)]): Int = {
      if (pending.isEmpty) acc
      else {
        val (c, r) = pending.head
        val (incrAcc, incrPending) = if (c == 0 || c == r)  (1, Nil)
        else if (c == 1 || c == r - 1)  (r, Nil)
        else  (0, List((c, r - 1), (c - 1, r - 1)))

        visit(acc + incrAcc, pending.tail ++ incrPending)
      }
    }

    visit(0, List((c, r)))
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def visit(opened: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) opened == 0
      else if (chars.head == ')' && opened == 0) false
      else {
        val incr = chars.head match {
          case ')' => -1
          case '(' => 1
          case _ => 0
        }
        visit(opened + incr, chars.tail)
      }
    }

    visit(0, chars)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def sum(change: List[Int]): Int =
      change.zip(coins).map { case (a, b) => a * b }.sum

    case class Context(index: Int, change: List[Int])

    def incr(change: List[Int], index: Int, qnt: Int = 1): List[Int] =
      change.zipWithIndex.iterator.map { case (i, i1) => if (i1 == index) i + qnt else i }.toList

    @scala.annotation.tailrec
    def howManyChanges(acc: Int, contexts: List[Context], coins: List[Int]): Int = {

      if (contexts.isEmpty) acc
      else {
        val context = contexts.head
        val total = sum(context.change)
        val incrAcc = if (total == money) 1 else 0

        val nextContexts = if (total < money) {
          val incrContexts = Context(context.index, incr(context.change, context.index)) +: contexts.tail
          if (context.index + 1 < context.change.length) incrContexts :+ Context(context.index + 1, context.change)
          else incrContexts
        } else contexts.tail

        howManyChanges(acc + incrAcc, nextContexts, coins)
      }
    }

    val initalChangeSeed = List.fill(coins.length)(0)
    howManyChanges(0, List(Context(0, initalChangeSeed)), coins)
  }
}
