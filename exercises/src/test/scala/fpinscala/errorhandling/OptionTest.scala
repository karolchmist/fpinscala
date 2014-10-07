package fpinscala.errorhandling

/**
 *
 */
class OptionTest extends org.specs2.mutable.Specification {
  "Option" should {
    "filter" in {
      Some(1) filter(_ % 2 == 0) mustEqual None
      Some(2) filter(_ % 2 == 0) mustEqual Some(2)
      None filter(Nil) mustEqual None
    }
    "lift" in {
      def lift[A,B](f:A=>B) : Option[A] => Option[B] = _ map(f)
      val abs0 = lift(math.abs)
      abs0(Some(-3)) mustEqual Some(3)
      abs0(None) mustEqual None
    }
    "map2" in {
      Option.map2(Some(2), Some("a"))((a,b) => a + b) mustEqual Some("2a")
    }
  }
}
