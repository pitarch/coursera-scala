package scalashop

import org.junit.Rule
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BoxBlurKernelInterfaceSuite extends AnyFunSuite with Matchers {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  test("boxBlurKernel should produce the same image when radius is zero") {
    val img = new Img(1, 1)
    img(0, 0) shouldBe rgba(0, 0, 0, 0)
    val blurredPixel = boxBlurKernel(img, 0, 0, 0)
    blurredPixel shouldBe rgba(0, 0, 0, 0)
  }

  test("boxBlurKernel should blur") {
    val img = new Img(3, 3)
    for {i <- 0 until 3; j <- 0 until 3} img.update(i, j, rgba(i + 1, i + 1, i + 1, i + 1))
    val blurredPixel = boxBlurKernel(img, 1, 1, 1)
    green(blurredPixel) shouldBe 2
    red(blurredPixel) shouldBe 2
    blue(blurredPixel) shouldBe 2
    alpha(blurredPixel) shouldBe 2
  }

  test("boxBlurKernel should return the correct value on the edge pixels of a 3x4 image with radius 1") {
    // boxBlurKernel(0, 2, 1) should be 13
    /*
    * y 0 1 2 3
    *   1
    *   2
     */
  }

}
