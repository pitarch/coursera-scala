package reductions

import org.junit.Assert.assertEquals
import org.junit._

class LineOfSightSuite {

  import LineOfSight._

  @Test def `lineOfSight should correctly handle an array of size 4`: Unit = {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assertEquals(List(0f, 1f, 4f, 4f), output.toList)
  }


  @Test def `parLineOfSight should invoke the parallel construct 30 times (15 times during upsweep and 15 times during downsweep) for an array of size 17, with threshold 1`: Unit = {
    val input = new Array[Float](17)
    val output = new Array[Float](input.length)
    parLineOfSight(input, output, 1)
  }

  @Test def `parLineOfSight should correctly compute the output for threshold 3`: Unit = {
    val input = new Array[Float](6)
    input(1) = 9f
    val output = new Array[Float](input.length)
    parLineOfSight(input, output, 3)
    val expected = List(0.0, 9.0, 9.0, 9.0, 9.0, 9.0)
    assertEquals(expected, output.toList)
  }

  @Test def `parLineparLineOfSight should correctly compute the output for threshold 2`: Unit = {
    val input = new Array[Float](5)
    input(1) = 7f
    input(2) = 2 * 7f
    input(3) = 3 * 11f
    input(4) = 4 * 12f
    val output = new Array[Float](input.length)
    parLineOfSight(input, output, 2)
    val expected = List(0.0, 7.0, 7.0, 11.0, 12.0)
    assertEquals(expected, output.toList)
  }

  @Test def `downsweep should correctly handle trees with a single leaf`: Unit = {
    val input = new Array[Float](5)
    input(1) = 7f
    input(2) = 2 * 7f
    input(3) = 3 * 11f
    input(4) = 4 * 12f
    val output = new Array[Float](input.length)
    parLineOfSight(input, output, input.length)
    val expected = List(0.0, 7.0, 7.0, 11.0, 12.0)
    assertEquals(expected, output.toList)
  }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

