package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    elem <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(elem, h)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    if(a > b) findMin(h) == b else findMin(h) == a
  }

  property("deleteMin1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sortedSequence1") = forAll { (h: H) =>
    val heap = deleteMin(h)
    if(isEmpty(heap)){
      true
    }
    else {
      ord.lt(findMin(h), findMin(heap))
    }
  }


  property("meld1") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = findMin(meld(h1, h2))
    if(min1 > min2) min == min2 else min == min1
  }
}
