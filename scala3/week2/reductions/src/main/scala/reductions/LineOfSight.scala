package reductions

import org.scalameter._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    if (input.length > 0) output(0) = 0f
    var i = 1
    var max: Float = 0f
    while (i < input.length) {
      val tangent = input(i) / i
      if (tangent > max) max = tangent
      output(i) = max
      i = i + 1
    }
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {

    var maxTangent = 0f
    var i = from
    while (i < until) {
      val currentTangent: Float = if (i == 0) 0f else input(i) / i
      if (currentTangent > maxTangent) maxTangent = currentTangent
      i = i + 1
    }
    maxTangent
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   * returns the reduction tree for that part of the array.
   *
   * The reduction tree is a `Leaf` if the length of the specified part of the
   * array is smaller or equal to `threshold`, and a `Node` otherwise.
   * If the specified part of the array is longer than `threshold`, then the
   * work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
              threshold: Int): Tree = {

    if (end - from <= threshold) {
      val max = upsweepSequential(input, from, end)
//      println(s"upsweep Leaf: [$from, $end] => $max")
      Leaf(from, end, max)
    }
    else {
      val middle = from + (end - from) / 2
//      println(s"parallel upsweep: left: [$from, $middle]  right: [$middle, $end]")
      val (tL, tR) = parallel(upsweep(input, from, middle, threshold), upsweep(input, middle, end, threshold))
      Node(tL, tR)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   * `until`, and computes the maximum angle for each entry of the output array,
   * given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
                          startingAngle: Float, from: Int, until: Int): Unit = {
//    println(s"downsweepSequential: [$from, $until] => startingAngle: $startingAngle")
    var i = from
    var latest = startingAngle
    while (i < until) {
      val theAngle = if (i == 0) 0f else input(i) / i
      latest = latest.max(theAngle)
      output(i) = latest
      i = i + 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   * reduction `tree` in parallel, and then calls `downsweepSequential` to write
   * the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
                tree: Tree): Unit = {
    tree match {
      case Node(left, right) =>
//        println(s"parallel downsweep: left max: ${left.maxPrevious}  right max: ${right.maxPrevious}")
        parallel(downsweep(input, output, startingAngle.min(left.maxPrevious), left), downsweep(input, output, left.maxPrevious, right))
      case Leaf(from, until, maxPrevious) =>
//        println(s"downsweep/leaf: [$from, $until]/$maxPrevious")
        downsweepSequential(input, output, startingAngle, from, until)
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float], threshold: Int): Unit = {
    val tree = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, 0f, tree)
    output(0) = 0f
  }
}
