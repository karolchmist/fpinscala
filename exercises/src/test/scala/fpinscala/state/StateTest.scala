package fpinscala.state

import fpinscala.state.RNG.{Rand, Simple}
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 *
 */
class StateTest extends Specification with ScalaCheck {
  "RNG" should {
    "produce nonNegativeInt" ! prop { (s:Int) =>
      val (i1, rng2) = RNG.nonNegativeInt( Simple (s) )
      i1 >= 0 && i1 <= Int.MaxValue
    }
    "produce double" ! prop { (s: Int) =>
      val (i1, rng2) = RNG.double (Simple (s) )
      i1 >= 0.0 && i1 < 1.0
    }
  }
}
