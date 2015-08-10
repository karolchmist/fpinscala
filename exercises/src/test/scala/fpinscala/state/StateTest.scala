package fpinscala.state

import fpinscala.state.RNG.{Rand, Simple}
import org.scalacheck.{Gen, Prop}
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
    import scala.math.Numeric.IntIsIntegral
    val allNums = Gen.oneOf(Gen.posNum, Gen.negNum)
    "ints" ! Prop.forAll(Gen.posNum, allNums) { (count: Int, s: Int) =>
      val (is, rng2) = RNG.ints(count)(Simple (s) )
      is.size == count
    }
    "doubleMap" ! prop { (s: Int) =>
      val (d, rng) = RNG.doubleMap(Simple(s))
      testDouble(d)
    }
    "map2" ! prop { (s: Int) =>
      val ((i,d), rng):((Int, Double), RNG) = RNG.map2(RNG.int, RNG.double)((_,_))(Simple(s))
      testInt(i) && testDouble(d)
    }
    "nonNegativeLessThan" ! Prop.forAll(Gen.posNum) { (s: Int) =>
      val (r, _) = RNG.nonNegativeLessThan(s)(Simple(1))
      r >= 0 && r < s
    }
    "mapByFlatMap" ! prop { (s: Int) =>
      val (r, _) = RNG.mapByFlatMap(RNG.unit(s))(_ * 2)(Simple(1))
      r == s * 2
    }
    "map2ByFlatMap" ! prop { (n1: Int, n2:Int) =>
      val (r, _) = RNG.map2ByFlatMap(RNG.unit(n1), RNG.unit(n2))((p1,p2) => p1 + p2)(Simple(1))
      n1 + n2 <= r
    }
  }

//  "State" should {
//    "unit" ! prop { (n:Int) =>
//      State((n,(_:Any))).unit(2) ==== State(s => (2,s))
//
//    }
//  }

  def testInt(i: Int) : Boolean = {
    // not a very smart test....
    i >= Int.MinValue && i <= Int.MaxValue
  }

  def testPositiveInt(i: Int): Boolean = {
    i >= 0 && i <= Int.MaxValue
  }

  def testDouble(d: Double): Boolean = {
    d >= 0.0 && d < 1.0
  }
}
