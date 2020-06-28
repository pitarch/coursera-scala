package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal[Double] {
      val aVal = a()
      val bVal = b()
      val cVal = c()
      bVal * bVal - 4 * aVal * cVal
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal[Set[Double]] {
      val deltaVal = delta()
      val aVal = a()
      val bVal = b()
      if (deltaVal < 0.0) Set.empty
      else if (deltaVal == 0.0) {
        Set((-bVal) / (2 * aVal))
      } else {
        val sq = Math.sqrt(deltaVal)
        // (-b ± √Δ) / 2a
        Set( (-bVal + sq) / (2 * aVal), (-bVal - sq) / (2 * aVal) )
      }
    }
  }
}
