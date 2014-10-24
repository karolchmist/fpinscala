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
    "headOption" in {
      Stream.empty.headOption mustEqual Option.empty
      Stream(1, 2, 4,8).headOption mustEqual Option(1)
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
      Stream.from(0).take(3).toList mustEqual List(0,1,2)
      Stream.from(5).take(3).toList mustEqual List(5,6,7)
    }
    "fibo" in {
      Stream.fibo.take(1).toList mustEqual List(0)
      Stream.fibo.take(2).toList mustEqual List(0,1)
      Stream.fibo.take(3).toList mustEqual List(0,1,1)
      Stream.fibo.take(4).toList mustEqual List(0,1,1,2)
      Stream.fibo.take(5).toList mustEqual List(0,1,1,2,3)
      Stream.fibo.take(6).toList mustEqual List(0,1,1,2,3,5)
      Stream.fibo.take(7).toList mustEqual List(0,1,1,2,3,5,8)
    }
    "unfold" in {
      Stream.unfold(1)(x => Some((x.toString,x*3))).take(3).toList mustEqual List("1","3", "9")
    }
    "fiboUnfold" in {
      Stream.fiboUnfold.take(1).toList mustEqual List(0)
      Stream.fiboUnfold.take(2).toList mustEqual List(0,1)
      Stream.fiboUnfold.take(3).toList mustEqual List(0,1,1)
      Stream.fiboUnfold.take(4).toList mustEqual List(0,1,1,2)
      Stream.fiboUnfold.take(5).toList mustEqual List(0,1,1,2,3)
      Stream.fiboUnfold.take(6).toList mustEqual List(0,1,1,2,3,5)
    }
    "fromUnfold" in {
      Stream.fromUnfold(0).take(3).toList mustEqual List(0,1,2)
      Stream.fromUnfold(5).take(4).toList mustEqual List(5,6,7,8)
    }
    "constantUnfold" in {
      Stream.constantUnfold(0).take(3).toList mustEqual List(0,0,0)
      Stream.constantUnfold(5).take(4).toList mustEqual List(5,5,5,5)
    }
    "onesUnfold" in {
      Stream.onesUnfold.take(3).toList mustEqual List(1,1,1)
      Stream.onesUnfold.take(4).toList mustEqual List(1,1,1,1)
    }
    "mapUnfold" in {
      Stream.empty.mapUnfold( (_:Any) => throw new IllegalStateException).toList mustEqual List()
      Stream(1, 2, 4,8).mapUnfold(_ + 10 ).toList mustEqual List(11,12,14,18)
      Stream(1, 2, 4,8).mapUnfold(_ + 10 ).mapUnfold(_.toString).toList mustEqual List("11","12","14","18")
    }
    "takeUnfold" in {
      Stream.empty.takeUnfold(2).toList mustEqual List()
      Stream(1, 2, 4,8,10,12).takeUnfold(0).toList mustEqual List()
      Stream(1, 2, 4,8,10,12).takeUnfold(2).toList mustEqual List(1,2)
    }
      Stream.empty.takeWhile2( (_:Any) =>true).toList mustEqual List()
    "takeWhileUnfold" in {
      Empty.takeWhileUnfold( _ => true ).toList mustEqual List()
      Stream(1, 2, 4,8).takeWhileUnfold( _ < 4 ).toList mustEqual List(1,2)
      Stream(1, 2, 4,8).takeWhileUnfold( _ < 9 ).toList mustEqual List(1,2,4,8)
    }
    "zipWithUnfold" in {
      Empty.zipWithUnfold(Stream.from(5)).take(4).toList mustEqual List()
      Stream.from(5).zipWithUnfold(Empty).take(4).toList mustEqual List()
      Stream.from(5).zipWithUnfold(Stream.from(10).map(_.toString)).take(3).toList mustEqual List((5,"10"),(6,"11"),(7,"12"))
    }
    "zipAllUnfold" in {
      Stream(1).zipAllUnfold(Stream(5)).take(1).toList mustEqual List((Some(1),Some(5)))
      Stream(1).zipAllUnfold(Stream.from(5)).take(3).toList mustEqual List((Some(1),Some(5)),(None,Some(6)),(None,Some(7)))
      Stream.from(5).zipAllUnfold(Empty).take(2).toList mustEqual List((Some(5),None),(Some(6),None))
    }

  }
}
