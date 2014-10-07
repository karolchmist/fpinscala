package fpinscala.errorhandling

import org.specs2.mutable.Specification

class EitherTest extends Specification {
  "Either" should {
    "map" in {
      Left("wrong").map{a:Int=>a*2} mustEqual Left("wrong")
      Right(3).map{_*2} mustEqual Right(6)
    }
  }
}
