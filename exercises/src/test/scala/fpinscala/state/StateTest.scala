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
    "nonNegativeInt" ! prop { (s:Int) =>
      val (i1, rng2) = RNG.nonNegativeInt( Simple (s) )
      testPositiveInt(i1)
    }
    "double" ! prop { (s: Int) =>
      val (i1, rng2) = RNG.double (Simple (s) )
      testDouble(i1)
    }
    "intDouble" ! prop { (s: Int) =>
      val ((i,d), rng2) = RNG.intDouble (Simple (s) )
      testDouble(d) && testInt(i)
    }
    "doubleInt" ! prop { (s: Int) =>
      val ((d,i), rng2) = RNG.doubleInt(Simple (s) )
      testDouble(d) && testInt(i)
    }
    "double3" ! prop { (s: Int) =>
      val ((d1,d2,d3), rng2) = RNG.double3(Simple (s) )
      testDouble(d1) && testDouble(d2) && testDouble(d3)
    }
  }

  def testInt(i: Int) : Boolean = {
    // not very smart test....
    i >= Int.MinValue && i <= Int.MaxValue
  }

  def testPositiveInt(i: Int): Boolean = {
    i >= 0 && i <= Int.MaxValue
  }

  def testDouble(i: Double): Boolean = {
    i >= 0.0 && i < 1.0
  }
}
