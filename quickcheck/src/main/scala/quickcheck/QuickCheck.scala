package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(element, heap)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("inTwoElemInEmpty") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val smaller = if (a < b) a else b
    findMin(h) == smaller
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("inDelMin") = forAll { a: Int =>
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    h1 == empty
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meldMin") = forAll { (a: H, b: H) =>
    val min1 = findMin(a)
    val min2 = findMin(b)
    val m = meld(a, b)
    val minMeld = findMin(m)
    minMeld == min1 || minMeld == min2
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("checkSorted") = forAll { h: H =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val xs = remMin(h, Nil)
    xs == xs.sorted
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2, meld the results. Compare two melds by comparing sequences of ranks.
  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def meldMinMove(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: meldMinMove(deleteMin(ts), as)
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = meldMinMove(meld1, Nil)
    val xs2 = meldMinMove(meld2, Nil)
    xs1 == xs2
  }


}
