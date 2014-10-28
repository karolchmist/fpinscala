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
      testInt(i1)
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

  def testDouble(i1: Double): Boolean = {
    i1 >= 0.0 && i1 < 1.0
  }

  def testInt(i1: Int): Boolean = {
    i1 >= 0 && i1 <= Int.MaxValue
  }
}
