package example

import scala.annotation.tailrec


object Sandbox extends App {

  def sumInts(a: Int, b: Int): Int = {
    if (a > b) 0 else a + sumInts(a + 1, b)
  }

  def cube(x: Int) = x * x * x

  def sumCubes(a: Int, b: Int): Int = {
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)
  }

//  def sum(f: Int => Int, a: Int, b: Int): Int = {
//    if (a > b) 0 else f(a) + sum(f, a + 1, b)
//  }

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  println(sum(x => x, 0, 5))
}