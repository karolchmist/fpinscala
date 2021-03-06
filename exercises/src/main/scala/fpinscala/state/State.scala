package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,rng2) = rng.nextInt
    (i.abs,rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i,rng2) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),rng2) = intDouble(rng)
    ((d,i),rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (i, rng2) = rng.nextInt
      val (is, rng3) = ints(count - 1)(rng2)
      (i::is,rng3)
    }
  }

  def doubleMap: Rand[Double] = map(nonNegativeInt)(_.toDouble/Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    RNG => {
      val (a,rng) = f(RNG)
      g(a)(rng)
    }
  }

  def nonNegativeLessThan(n:Int) : Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if(i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }

    def mapByFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(f andThen unit)

    def map2ByFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra){ a =>
        map(rb) { b =>
          f(a,b)
        }
      }
}

case class State[S,+A](run: S => (A, S)) {
  def unit[A](a:A) : State[S, A] =
    State( s => (a,s))
  def map[B](f: A => B): State[S, B] =
    State[S,B]{ s =>
      run(s) match {
        case (a,s1) => (f(a), s1)
      }
    }
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
