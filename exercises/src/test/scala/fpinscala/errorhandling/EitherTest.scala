package fpinscala.errorhandling

import org.specs2.mutable.Specification

class EitherTest extends Specification {
  "Either" should {
    "map" in {
      val toStr: (Any) => String = _.toString
      val twice: (Int) => Int = _ * 2

      Left("error") map twice mustEqual Left("error")
      Right(3) map twice map toStr mustEqual Right("6")
    }
    "flatMap" in {
      def twice(a:Int):Either[_,Int] = Right(a*2)

      (Right(3) flatMap twice) mustEqual Right(6)
      (Left("error") flatMap twice) mustEqual Left("error")
    }
    "orElse" in {
      (Right(3) orElse Right(4)) mustEqual Right(3)
      (Left("error") orElse Right(4)) mustEqual Right(4)
    }
    "map2" in {
      def f(a:Int, b:Double):String = (a+b).toString
      Right(2).map2(Right(4.0))(f) mustEqual Right("6.0")
    }
      "traverse" in {
        def positiveToRight(n:Int) = if(n > 0) Right(n) else Left(n.toString)

        Either.traverse(List(1))(positiveToRight) ==== Right(List(1))
        Either.traverse(List(1,2))(positiveToRight) ==== Right(List(1,2))
        Either.traverse(List(1,2,-3))(positiveToRight) ==== Left("-3")
      }
    "sequence" in {
      Either.sequence(List()) mustEqual Right(Nil)
      Either.sequence(List(Right(1))) mustEqual Right(List(1))
      Either.sequence(List(Right(1), Right(2))) mustEqual Right(List(1,2))
      Either.sequence(List(Right(1), Left("error1"), Left("error2"))) mustEqual Left("error1")
      Either.sequence(List(Left("error"), Right(2))) mustEqual Left("error")
    }
  }
}
