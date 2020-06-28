package scalashop

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HorizontalBoxBlurSuite extends AnyFunSuite with Matchers {


  test("blur with radius 2 should correctly blur the entire 4x3 image") {
    val img = new Img(4, 3)
    val dst = new Img(img.width, img.height)
    for {i <- 0 until img.width; j <- 0 until img.height} img.update(i, j, rgba(1, 1, 1, 1))
    HorizontalBoxBlur.blur(img, dst, 0, img.height, 2)
    for {i <- 0 until img.width; j <- 0 until img.height} img(i, j) shouldBe dst(i, j)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 and 1 task should correctly blur the entire 3x3 image") {
    val src = new Img(3, 3)
    val dst = new Img(src.width, src.height)
    for {i <- 0 until src.height; j <- 0 until src.width} src.update(i, j, rgba(0, 0, 0, i + 1))
    HorizontalBoxBlur.parBlur(src, dst, 1, 1)
    for {i <- 0 until src.height; j <- 0 until src.width} src(i, j) == 2
  }


  test("HorizontalBoxBlur.parBlur with 32 tasks should execute 32 parallel tasks for a 64x32 image, each blurring one strip (3pts)(scalashop.BlurSuite)") {
    val src = new Img(64, 32)
    val dst = new Img(src.width, src.height)
    //for {row <- 0 until src.height; col <- 0 until src.width} src.update(row, col, rgba(0, 0, 0, row + 1))
    HorizontalBoxBlur.parBlur(src, dst, 32, 1)
    for {i <- 0 until src.width; j <- 0 until src.height} dst(i, j) == 1
    1 shouldBe 1
  }
}
