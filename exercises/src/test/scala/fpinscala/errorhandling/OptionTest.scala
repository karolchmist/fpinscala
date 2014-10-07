package fpinscala.errorhandling

import org.specs2.mutable.Specification

class OptionTest extends Specification {
  "Options" should {
    "work" in {
      Option.mean(List(2,4,9)) mustEqual Some(5)
    }
    "map" in {
      val toStr: (Any) => String = _.toString
      val twice: (Int) => Int = _ * 2

      Some(3) map twice mustEqual Some(6)
      Some(3) map twice map toStr mustEqual Some("6")
      None map toStr mustEqual None
      (None map twice map toStr) mustEqual None
    }
    "getOrElse" in {
      Some(3) getOrElse(4) mustEqual 3
      None getOrElse(4) mustEqual 4
    }
    "flatMap" in {
      val flatTwice: (Int) => Option[Int] = a => Some(a * 2)
      (Some(3) flatMap flatTwice) mustEqual Some(6)
      (None flatMap flatTwice) mustEqual None
    }
    "orElse" in {
      (Some(3) orElse Some(4)) mustEqual Some(3)
      (None orElse Some(4)) mustEqual Some(4)
    }
    "filter" in {
      val flatTwice: (Int) => Some[Int] = a => Some(a * 2)
      (Some(3) filter {_ % 2 == 0}) mustEqual None
      (Some(4) filter {_ % 2 == 0}) mustEqual Some(4)
      ((None:Option[Int]) filter {_ % 2 == 0}) mustEqual None
    }
  }
}
