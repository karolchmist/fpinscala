package fpinscala.laziness

import org.specs2.mutable.Specification

class StreamTest extends Specification {
  "Stream" should {
    "toList" in {
      Stream.empty.toList mustEqual List()
      Stream(1, 2, 4).toList mustEqual List(1, 2, 4)
    }
    "take" in {
      Stream.empty.take(2).toList mustEqual List()
      Stream(1, 2, 4,8).take(2).toList mustEqual List(1,2)
    }
    "drop" in {
      Stream.empty.drop(2).toList mustEqual List()
      Stream(1, 2, 4,8).drop(2).toList mustEqual List(4,8)
    }
    "takeWhile" in {
      Stream.empty.takeWhile( (_:Any) =>true).toList mustEqual List()
      Stream(1, 2, 4,8).takeWhile( _ < 4 ).toList mustEqual List(1,2)
      Stream(1, 2, 4,8).takeWhile( _ < 9 ).toList mustEqual List(1,2,4,8)
    }
    "forAll" in {
      Stream.empty.forAll( (_:Any) => true) mustEqual true
      Stream(1, 2, 4,8).forAll( _ < 4 ) mustEqual false
      Stream(1, 2, 4,8).forAll( _ < 9 ) mustEqual true
    }
    "takeWhile2" in {
      Stream.empty.takeWhile2( (_:Any) =>true).toList mustEqual List()
      Stream(1, 2, 4,8).takeWhile2( _ < 4 ).toList mustEqual List(1,2)
      Stream(1, 2, 4,8).takeWhile2( _ < 9 ).toList mustEqual List(1,2,4,8)
    }
    "map" in {
      Stream.empty.map( (_:Any) => throw new IllegalStateException).toList mustEqual List()
      Stream(1, 2, 4,8).map(_ + 10 ).toList mustEqual List(11,12,14,18)
      Stream(1, 2, 4,8).map(_ + 10 ).map(_.toString).toList mustEqual List("11","12","14","18")
    }
    "filter" in {
      Stream.empty.filter( (_:Any) => throw new IllegalStateException).toList mustEqual List()
      Stream(1, 3, 4,8, 9).filter(_ % 2 == 0 ).toList mustEqual List(4,8)
      Stream(1, 2, 4,8).map(_ + 10 ).filter(_ < 13).toList mustEqual List(11,12)
    }
    "append" in {
      Stream.empty.append(Stream(1,4,3)).toList mustEqual List(1,4,3)
      Stream(1,4,3).append(Stream.empty).toList mustEqual List(1,4,3)
      Stream(1, 9, 4).append(Stream(10,40,30)).toList mustEqual List(1, 9, 4, 10, 40, 30)
    }
    "flatMap" in {
      Stream.empty.flatMap( (_:Any) => throw new IllegalStateException).toList mustEqual List()
      Stream(1, 3, 4, 9).flatMap(a => Stream.cons(a,Empty)).toList mustEqual List(1, 3, 4, 9)
      Stream(1, 3, 4, 9).flatMap(a => Stream.cons(a.toString,Empty)).toList mustEqual List("1", "3", "4", "9")
    }
    "constant" in {
      Stream.constant(5).take(3).toList mustEqual List(5,5,5)
    }
    "from" in {
      Stream.from(5).take(3).toList mustEqual List(5,6,7)
    }
  }
}
