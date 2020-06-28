package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

//  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
//    h1 <- oneOf(const(empty), arbitrary[H])
//    h2 <- oneOf(const(empty), arbitrary[H])
//  } yield meld(h1, h2))


  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    a <- arbitrary[A]
    h1 <- arbitrary[H]
    h2 <- arbitrary[H]
  } yield meld(insert(a, h1), deleteMin(h2)))

//  lazy val genHeap: Gen[H] = const(empty)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

}
