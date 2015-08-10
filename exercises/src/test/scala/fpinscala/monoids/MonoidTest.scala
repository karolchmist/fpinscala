package fpinscala.monoids

import fpinscala.monoids.Monoid._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.scalacheck.{Gen, Prop}


/**
 * Created by karol on 14/05/15.
 */
class MonoidTest extends Specification with ScalaCheck {

  "monoid" should {

    "obey law of assiosiativity" ! prop { (a:Int, b:Int, c:Int) =>
      intAddition.op(a, intAddition.op(b,c)) mustEqual intAddition.op(intAddition.op(a,b), c)
    }
  }
}