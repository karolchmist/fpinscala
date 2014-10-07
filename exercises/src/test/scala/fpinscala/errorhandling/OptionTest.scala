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
    "sequence" in {
      Option.sequence(List()) mustEqual Some(Nil)
      Option.sequence(List(Some(1))) mustEqual Some(List(1))
      Option.sequence(List(Some(1), Some(2))) mustEqual Some(List(1,2))
      Option.sequence(List(Some(1), None)) mustEqual None
      Option.sequence(List(None, Some(2))) mustEqual None
    }
    "traverse" in {
      def Try[A](a: => A): Option[A] =
        try Some(a)
        catch { case e: Exception => None }

      Option.traverse(List("1", "4"))(a=>Try(a.toInt)) mustEqual Some(List(1,4))
      Option.traverse(List("1", "abc", "4"))(a=>Try(a.toInt)) mustEqual None
    }
    "sequence2" in {
      Option.sequence2(List()) mustEqual Some(Nil)
      Option.sequence2(List(Some(1))) mustEqual Some(List(1))
      Option.sequence2(List(Some(1), Some(2))) mustEqual Some(List(1,2))
      Option.sequence2(List(Some(1), None)) mustEqual None
      Option.sequence2(List(None, Some(2))) mustEqual None
    }

  }
}
