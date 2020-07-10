package barneshut

import org.junit._

import scala.collection._

class MyBarnesHutSuite {
  // test cases for quad tree

  import FloatOps._

  @Test def `Leaf with 0 body`: Unit = {
    val quad = Leaf(17.5f, 27.5f, 5f, Seq())

    assert(quad.mass ~= 0f, s"${quad.mass} should be 0")
    assert(quad.massX ~= quad.centerX, s"${quad.massX} should be ${quad.centerX}")
    assert(quad.massY ~= quad.centerY, s"${quad.massY} should be ${quad.centerY}")
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  @Test def `Leaf with 2 body`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b, b))

    assert(quad.mass ~= 246, s"${quad.mass} should be 246")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 2, s"${quad.total} should be 2")
  }

  @Test def `Leaf.insert(b) with no bodies` = {

    val leaf = Leaf(1, 1, 2, Seq())
    val body = new Body(1, 0.4f, 0.4f, 0, 0)
    val quad = leaf.insert(body)

    quad match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.isInstanceOf[Leaf])
        assert(nw.massX == body.x, "nw massX should be the body x")
        assert(nw.massY == body.y, "nw massX should be the body y")
        assert(nw.mass == body.mass, "nw mass should be the body mass")
        assert(ne.isInstanceOf[Empty], "ne should be empty")
        assert(sw.isInstanceOf[Empty], "sw should be empty")
        assert(se.isInstanceOf[Empty], "se should be empty")
      case _ => assert(false, "not a fork")
    }
  }

  @Test def `Leaf.insert(b) with an already body` = {

    val body1 = new Body(1, 0.4f, 0.4f, 0, 0)
    val leaf = Leaf(1, 1, 2, Seq(body1))
    val body2 = new Body(1, 0.2f, 0.2f, 0, 0)
    val quad = leaf.insert(body2)

    quad match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.isInstanceOf[Leaf])
        assert(nw.massX ~= 0.3f, "nw massX should be the 0.3")
        assert(nw.massY ~= 0.3f, "nw massX should be the 0.3")
        assert(nw.mass == body1.mass + body2.mass, "nw mass should be the sum of the masses of both bodies")
        assert(ne.isInstanceOf[Empty], "ne should be empty")
        assert(sw.isInstanceOf[Empty], "sw should be empty")
        assert(se.isInstanceOf[Empty], "se should be empty")
      case _ => assert(false, "not a fork")
    }
  }

  @Test def `Leaf.insert(b) with an already body returning a balanced fork` = {

    val body1 = new Body(1, 0.5f, 0.5f, 0, 0)
    val leaf = Leaf(1, 1, 2, Seq(body1))
    val body2 = new Body(1, 1.5f, 1.5f, 0, 0)
    val quad = leaf.insert(body2)

    assert(quad.mass == 2, "fork mass should be the sum of the masses of both bodies")
    assert(quad.massX == 1, "fork massX should be the 1")
    assert(quad.massY == 1, "fork massY should be the 1")

    quad match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.isInstanceOf[Leaf], "nw should be a leaf")
        assert(ne.isInstanceOf[Empty], "ne should be empty")
        assert(sw.isInstanceOf[Empty], "sw should be empty")
        assert(se.isInstanceOf[Leaf], "se should be leaf")
      case _ => assert(false, "not a fork")
    }
  }

  @Test def `Fork.insert(b) should insert recursively in the appropriate quadrant (2pts)` = {
    // nw of the Fork, Empty(15.0,25.0,5.0), should be a Leaf
    val body123 = new Body(123f, 18f, 26f, 0, 0)
    val body524 = new Body(123f, 24.5f, 25.5f, 0, 0)
    val body245 = new Body(245f, 22.4f, 41f, 0, 0)
    val seed = Empty(15, 45, 15)
    val result1 = seed.insert(body123)
    val result2 = result1.insert(body245)
    val result3 = result2.insert(body524)
    assert(result3.isInstanceOf[Fork])


  }

  @Test def `'SectorMatrix.$plus$eq' should add a body at (25,47) to the correct bucket of a sector matrix of size 100 (2pts)(barneshut.BarnesHutSuite)` = {

    val matrix = new SectorMatrix(new Boundaries, 100)
    val body = new Body(1, 25, 47, 0, 0)
    matrix += body
    assert(1 == 1)
  }
  @Test def `Body.updated should consider a Fork as opaque if it is far away` = {

  }

  @Test def `Fork with 4 leaves (2pts)` = {
    // assertion failed: 20.275 should be 21.324333f
    val nw = Leaf(1, 1, 1, Seq())
    val ne = Leaf(3, 1, 1, Seq())
    val sw = Leaf(1, 3, 1, Seq())
    val se = Leaf(3, 3, 1, Seq())
    val fork = Fork(nw, ne, sw, se)

  }

  @Test def `Leaf.insert(b) should return a new Fork if size > minimumSize (2pts)`: Unit = {

    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val leaf = Leaf(17.5f, 27.5f, 5f, Seq(b))
    assert(leaf.size > minimumSize, "leaf.size should be > minimumSize")
    val quad: Fork = leaf.insert(b).asInstanceOf[Fork]
    assert(quad.nw.isInstanceOf[Leaf], "leaf.insert(b) should return a new fork")
  }

  @Test def `'insert' should work correctly on a leaf with center (1,1) and size 2 (2pts)` = {

    /*
    expected:
    Fork(
    Leaf(0.5,0.5,1.0,List(barneshut.package$Body@4e3879b2)),
    Leaf(1.5,0.5,1.0,List(barneshut.package$Body@4a07e4ad)),
    Empty(0.5,1.5,1.0),
    Empty(1.5,1.5,1.0))

    found:
    Fork(
      Empty(0.5,0.5,1.0),
      Empty(1.5,0.5,1.0),
      Empty(0.5,1.5,1.0),
      Leaf(1.5,1.5,1.0,List(barneshut.package$Body@413dab3a)))
     */
    val leaf = Leaf(1, 1, 2, Seq())
    val body = new Body(1, 1, 1, 0, 0)
    val quad = leaf.insert(body)
    assert(quad.isInstanceOf[Fork])
  }


  //
  //  @Test def `Fork.insert(b) should insert recursively in the appropriate quadrant (2pts)`() = {
  //    /*
  //    Fork(
  //      Empty(10.0,30.0,10.0),
  //      Leaf(20.0,30.0,10.0,List(barneshut.package$Body@3a0b3a85)),
  //      Empty(10.0,40.0,10.0),
  //      Fork(
  //        Empty(17.5,37.5,5.0),
  //        Empty(22.5,37.5,5.0),
  //        Empty(17.5,42.5,5.0),
  //        Leaf(22.5,42.5,5.0,List(barneshut.package$Body@14a68550)))) should be a Fork where only ne changed
  //
  //     */
  //  }
}
