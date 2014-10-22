package fpinscala.laziness

import org.specs2.mutable.Specification

class StreamTest extends Specification {
  "Stream" should {
    "toList" in {
      Stream.empty.toList mustEqual List()
      Stream(1, 2, 4).toList mustEqual List(1, 2, 4)
    }
    "take" in {
      Stream.empty.take(2) mustEqual Empty
      Stream(1, 2, 4,8).take(2).toList mustEqual Stream(1,2).toList
    }
    "drop" in {
      Stream.empty.drop(2) mustEqual Empty
      Stream(1, 2, 4,8).drop(2).toList mustEqual Stream(4,8).toList
    }
    "takeWhile" in {
      println (Stream(1, 2, 4,8).takeWhile( _ < 4 ).toList)
      Stream.empty.takeWhile( (_:Any) =>true) mustEqual Empty
      Stream(1, 2, 4,8).takeWhile( _ < 4 ).toList mustEqual Stream(1,2).toList
    }
  }
}
