package example

import scala.annotation.tailrec

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}


object Week1Test extends App {


  def factorial(n: Int): Int = {
    @tailrec
    def loop(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else loop(acc * n, n-1)
    }
    loop(1,n)
  }


  println(factorial(14))

  // factorial(4) = 4 * factorial(3) = 4 * 3 factorial(2) = 4 * 3 * 2 factorial(1)
}