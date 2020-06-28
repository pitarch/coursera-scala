package week1

object Week1Test extends App {

  def abs(x:Double) = if (x < 0) -x else x

  def isGoodEnough(g: Double, x: Double) = {
    println(s"g: ${g}  -- x: ${x}")
    abs(x - g * g) < g * 0.0001
  }

  def improve(guess: Double, x: Double) =(guess + x / guess) / 2

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)


  def sqrt(x: Double) = sqrtIter(1.0, x)


  sqrt(0.001)

}
