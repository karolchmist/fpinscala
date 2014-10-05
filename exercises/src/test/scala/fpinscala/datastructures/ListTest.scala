package fpinscala.datastructures

import org.specs2.mutable.Specification

object ListTest extends Specification {
  "List" should {
    "assert" in {
      List.tail(List(1, 2, 3, 4)) mustEqual List(2, 3, 4)
      List.setHead(List(1, 2, 3, 4), 5) mustEqual List(5, 2, 3, 4)
      List.drop(List(1, 2, 3, 4), 2) mustEqual List(3, 4)
      List.dropWhile(List(1, 2, 3, 4), { x: Int => x < 3}) mustEqual List(3, 4)
      List.dropWhile(Nil, { x: Int => x < 3}) mustEqual Nil
      List.append(List(1, 2), List(3, 4)) mustEqual List(1, 2, 3, 4)
      List.init(List(1, 2, 3, 4)) mustEqual List(1, 2, 3)
      List.sum2(List(2, 3, 4)) mustEqual 9
      List.product2(List(2, 3, 4)) mustEqual 24
      List.length2(List(2, 3, 4)) mustEqual 3
      List.sumLeft(List(2, 3, 4)) mustEqual 9
      List.productLeft(List(2, 3, 4)) mustEqual 24
      List.lengthLeft(List(2, 3, 4)) mustEqual 3
      List.reverse(List(2, 3, 4)) mustEqual List(4, 3, 2)
      List.append2(List(2, 3, 4), List(10, 20, 30)) mustEqual List(2, 3, 4, 10, 20, 30)
      List.concat(List(List(10, 11), List(20, 21), List(30, 31, 32))) mustEqual List(10, 11, 20, 21, 30, 31, 32)
      List.incr(List(1, 2, 3)) mustEqual List(2, 3, 4)
      List.toStr(List(1, 2, 3)) mustEqual List("1", "2", "3")
      List.map(List(1, 2, 3)) { _.toString } mustEqual List("1", "2", "3")
      List.filter(List(1, 2, 3, 4, 6))(_ % 2 == 0) mustEqual List(2, 4, 6)
      List.flatMap(List(2, 3, 4)) { a => List(a, a)} mustEqual List(2, 2, 3, 3, 4, 4)
      List.filterUsingflatMap(List(1, 2, 3, 4, 6))(_ % 2 == 0) mustEqual List(2, 4, 6)
      List.sumLists(List(2, 3, 4),List(4, 7, 1)) mustEqual List(6, 10, 5)
      List.zipWith(List(2, 3, 4),List(4, 7, 1))(_ + _) mustEqual List(6, 10, 5)
    }
  }

}
