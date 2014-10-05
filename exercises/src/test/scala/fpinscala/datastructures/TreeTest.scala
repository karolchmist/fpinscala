package fpinscala.datastructures

import org.specs2.mutable.Specification

class TreeTest extends Specification {
  "Tree" should {
    "size" in {
      Tree.size(Leaf(1)) mustEqual 1
      Tree.size(Branch(Leaf(1), Leaf(2))) mustEqual 3
      Tree.size(Branch(Leaf(1), Branch(Leaf(4), Leaf(3)))) mustEqual 5
      Tree.size(Branch(Leaf(1), Leaf(2))) mustEqual 3
    }
    "maximum" in {
      val t = Branch(Branch(Leaf(1), Leaf(7)), Branch(Leaf(4), Leaf(5)))
      Tree.maximum(t) mustEqual 7
    }
    "depth" in {
      val t = Branch(
          Branch(Leaf(1), Leaf(7)),
          Branch(Leaf(4),
            Branch(Leaf(5),Leaf(6))))

      Tree.depth(Leaf(9)) mustEqual 1
      Tree.depth(Branch(Leaf(4),Leaf(5))) mustEqual 2
      Tree.depth(t) mustEqual 4
    }
    "map" in {
      Tree.map(Branch(Branch(Leaf(3),Leaf(6)), Leaf(5)))(_.toString) mustEqual Branch(Branch(Leaf("3"),Leaf("6")), Leaf("5"))
    }
    "fold" in {
      val b = Branch(Branch(Leaf(3),Leaf(6)), Leaf(5))
      Tree.fold(b)(_ => 1)(1 + _ + _) mustEqual 5 // size
      Tree.fold(b)(a=>a)(_ max _) mustEqual 6 // max
      Tree.fold(b)(_ => 1)((a,b)=>1 + (a max b)) mustEqual 3 // depth
    }
  }
}
