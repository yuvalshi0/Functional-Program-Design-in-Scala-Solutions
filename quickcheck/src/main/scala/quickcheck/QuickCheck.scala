package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- Arbitrary.arbitrary[A]
    m <- oneOf(Gen.const(empty), genHeap)
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("delete min 2 elems results empty") =
    forAll { (a: Int, b: Int) =>
      val h1 = insert(a, insert(b, empty))
      deleteMin(deleteMin(h1)) == empty
    }

  property("gen3") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("order of mins") = forAll { (h:H) =>
    toList(h).zip(toList(h).drop(1)).forall {
      case (x, y) => x <= y
    }
  }

  property("melding 3 times and deleting 3 mins, next min are equal") =
    forAll { (h: H) =>
      val hm = meld(meld(h, h), h)
      val h1 = deleteMin(deleteMin(deleteMin(hm)))
      val h2 = deleteMin(h)
      isEmpty(h2) || findMin(h1) == findMin(h2)
    }

  property("Sorted sequence when deleting min") =
    forAll { (h: H) =>
      val seq = elemSeq(h, Seq())
      seq == seq.sorted
    }

  property("meld empty results same heap") =
    forAll { (h: H) =>
      meld(h, empty) == h
    }
  
  def elemSeq(h: H, s: Seq[A]): Seq[A] = {
    if (isEmpty(h)) s
    else elemSeq(deleteMin(h), s ++ Seq(findMin(h)))
  }

  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))

  property("gen5") = forAll { (h1: H, h2:H) =>
    findMin(meld(h1,h2)) == Math.min(findMin(h1),findMin(h2))
  }
}
