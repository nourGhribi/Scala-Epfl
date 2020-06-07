package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(Gen.const(empty),genHeap)
  } yield insert(i,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("gen2") = forAll { (e1: Int,e2 : Int) =>
    val h = insert(e1,insert(e2,empty))
    findMin(h) == min(e1,e2)

  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("sorted") = forAll { h: H =>

    def sorted(h:H):Boolean = if(isEmpty(h)) true
    else {val min = findMin(h)
          val nh = deleteMin(h)
          isEmpty(nh) || (min <= findMin(nh) && sorted(nh))}

    sorted(h)

  }

  property("meld") = forAll { (h1: H ,h2: H)  =>

    def heapEq(h1:H,h2:H):Boolean = {
      if(isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        (m1 == m2) && heapEq(deleteMin(h1), deleteMin(h2))
      }
    }

    heapEq(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))

  }



}
